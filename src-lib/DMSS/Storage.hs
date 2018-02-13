-- |
-- Module      : DMSS.Storage
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System storage module
--
module DMSS.Storage ( storeCheckIn
                    , listCheckIns
                    , latestCheckIns
                    , verifyPublicCheckIn
                    , storeUser
                    , listUsers
                    , getUserKey
                    , removeUser
                    , CheckInId
                    , UserId
                    , CheckInProof
                    , mkCheckInProof
                    , unCheckInProof
                    , Name (..)
                    , Password (..)
                    , HashSalt, toHashSalt, fromHashSalt
                    , BoxKeypairStore (..)
                    , SignKeypairStore (..)
                    , dbConnectionString
                    , User ( userHashSalt, userName
                           , userBoxKeypairStore, userSignKeypairStore
                           )
                    , runStorage
                    , runStoragePool
                    , StorageT
                    )
  where

import           DMSS.Config ( localDirectory )
import           DMSS.Common ( getCurrentTimeInSeconds, toUTCTime )
import           DMSS.Crypto ( decodeSignPublicKey, toSigned )
import           DMSS.Storage.Types
import           DMSS.Storage.TH

import qualified Crypto.Lithium.Sign as S

import qualified Database.Persist.Sqlite as P
import           Database.Esqueleto
import           Data.Text ( pack
                           , unpack
                           )
import           Data.Time.Clock ( UTCTime )

import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Logger ( NoLoggingT
                                      , runStdoutLoggingT
                                      )
import           Control.Monad.Trans.Resource ( ResourceT )

dbConnectionString :: IO String
dbConnectionString = localDirectory >>= \ld -> pure $ ld ++ "/dmss.sqlite"

type StorageT = SqlPersistT (NoLoggingT (ResourceT IO))

-- | Store User information
storeUser :: Name           -- ^ Username
          -> HashSalt       -- ^ Hash and salt for deriving symm key
          -> BoxKeypairStore   -- ^ Box keypair encrypted
          -> SignKeypairStore   -- ^ Box keypair encrypted
          -> StorageT (Key User)
storeUser n h bkp skp = do
  t <- liftIO getCurrentTimeInSeconds
  insert $ User n h bkp skp t

removeUser :: Name  -- ^ User to delete by name
           -> StorageT ()
removeUser n = do
  -- Delete all checkins associated with UserKey
  mUserKey <- getBy $ UniqueName n
  case mUserKey of
    Nothing  -> error "Couldn't find User"
    (Just uk) -> P.deleteWhere [ UserId P.==. (entityKey uk) ]
  -- Then delete UserKey
  deleteBy $ UniqueName n

-- | Get UserKey ID
getUserKey :: Name    -- ^ User's name
           -> StorageT (Maybe (Key User))
getUserKey n = do
  maybeUser <- getBy $ UniqueName n
  maybe
    (pure Nothing)
    (\(Entity userId _) -> pure $ Just userId)
    maybeUser

-- | List the last `Int` users sorted by date
listUsers :: Int
          -> StorageT [User]
listUsers i = do
  s <- select $
         from $ \c -> do
           limit (toEnum i)
           orderBy [desc (c ^. UserCreated)]
           return c
  return (entityVal <$> s)

-- | Store a CheckIn
storeCheckIn :: Name          -- ^ Name of user
             -> CheckInProof  -- ^ Raw checkin verification proof
             -> StorageT (Either String (Key CheckIn))
storeCheckIn n (CheckInProof rawCheckInData) = do
  t <- liftIO $ getCurrentTimeInSeconds
  m <- getUserKey n
  maybe (pure $ Left "Could not find users name in DB")
    (\i -> do
      res <- insert $ CheckIn i rawCheckInData t
      pure $ Right res) m

-- | List the last `Int` checkins sorted by date 
listCheckIns :: Name -> Int
             -> StorageT ([(CheckInProof, UTCTime)])
listCheckIns n i = do
  s <- select $
         from $ \(u, c) -> do
           where_ ( u ^. UserName ==. val n
                &&. u ^. UserId ==. c ^. CheckInUserId
                  )
           limit (toEnum i)
           orderBy [desc (c ^. CheckInCreated)]
           return c
  return $ ((,) <$> CheckInProof . checkInRaw_data
                <*> toUTCTime . checkInCreated
           ) . entityVal <$> s

-- | Get latest checkins for all users
-- Returns checkins which have the possibility of being valid for
-- the users checkin time period preferences
latestCheckIns :: StorageT [(Name, [CheckInProof])]
latestCheckIns = do
  -- Get all users
  s <- select $ from $ \u -> do return u

  -- Get CheckInProof for reach user
  traverse (\u -> do
        let n = userName $ entityVal u
        -- TODO: Should actually get checkins based on time
        cs <- listCheckIns (userName $ entityVal u) 10
        return (n, (fst <$> cs))
    ) s

verifyPublicCheckIn :: Name           -- ^ Users Name
                    -> CheckInProof   -- ^ Public CheckIn
                    -> StorageT Bool  -- ^ True if the checkin can be verified
verifyPublicCheckIn n c = do
  -- Get public key for user
  maybeUser <- getBy $ UniqueName n
  let mPK = case ( decodeSignPublicKey . userSignKeypairStore . entityVal )
                   <$> maybeUser of
              Just (Right a) -> Just a
              _              -> Nothing

  -- Verify signature
      mM = S.openSigned <$> mPK <*> (Just (toSigned c))

  case mM of
    Just mM' -> case mM' of
      Just _ -> return True -- if m /= (pack "") then return True else return False
      Nothing -> return False
    Nothing -> return False

-- | Run storage actions with no logging, no pooling and silent migration
runStorage :: StorageT a -> IO a
runStorage action = dbConnectionString >>= \c -> P.runSqlite (pack c) $ do
  _ <- runMigrationSilent migrateAll
  action

-- | Run storage actions with stdout logging, pooling and stdout migration
runStoragePool :: StorageT a -> IO a
runStoragePool action = dbConnectionString >>= \c -> runStdoutLoggingT $ P.withSqlitePool (pack c) 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    stuff <- runMigrationSilent migrateAll
    liftIO $ mapM_ (putStrLn . unpack) stuff
    action
