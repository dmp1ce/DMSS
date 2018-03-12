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
                    , storePeer
                    , listPeers
                    , removePeer
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
                    , Port (Port), Host (Host)
                    )
  where

import           DMSS.Config ( localDirectory )
import           DMSS.Common ( toUTCTime )
import           DMSS.Crypto ( decodeSignPublicKey, toSigned )
import           DMSS.Storage.Types
import           DMSS.Storage.TH

import qualified Crypto.Lithium.Sign as S

import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import qualified Database.Persist.Sqlite as P
import qualified Database.Persist.Class as PC (delete)
import           Database.Esqueleto
import           Data.Text ( pack )
import           Data.Pool (Pool)
import           Data.Int (Int64)
import           Data.Time.Clock ( UTCTime, NominalDiffTime
                                 , addUTCTime, getCurrentTime )

import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Logger ( NoLoggingT )
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
  t <- liftIO getCurrentTime
  insert $ User n h bkp skp (UTCTimeStore t)

removeUser :: Name  -- ^ User to delete by name
           -> StorageT ()
removeUser n = do
  -- Delete all checkins associated with UserKey
  mUserKey <- getBy $ UniqueName n
  case mUserKey of
    Nothing  -> error "Couldn't find User"
    (Just uk) -> P.deleteWhere [ UserId P.==. entityKey uk ]
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
  t <- liftIO getCurrentTime
  m <- getUserKey n
  maybe (pure $ Left "Could not find users name in DB")
    (\i -> do
      res <- insert $ CheckIn i rawCheckInData (UTCTimeStore t)
      pure $ Right res) m

-- | List all checkins since `NominalDiffTime` from now
--   `Nothing` for time means return all checkins
listCheckIns :: Name                  -- ^ Owner of checkins
             -> Maybe NominalDiffTime -- ^ Timespan before now to list checkins
             -> StorageT [(CheckInProof, UTCTime)]
listCheckIns n mT = do
  ct <- liftIO getCurrentTime
  let cutoff d = UTCTimeStore $ addUTCTime (negate d) ct
      timeZero = UTCTimeStore (toUTCTime 0)
  s <- select $
         from $ \(u, c) -> do
           where_ ( u ^. UserName ==. val n
                &&. u ^. UserId ==. c ^. CheckInUserId
                &&. c ^. CheckInCreated >=. val (maybe timeZero cutoff mT)
                  )
           orderBy [desc (c ^. CheckInCreated)]
           return c
  return $ ((,) <$> CheckInProof . checkInRaw_data
                <*> unUTCTimeStore . checkInCreated
           ) . entityVal <$> s

-- | Get latest checkins for all users
-- Returns checkins which have the possibility of being valid for
-- the users checkin time period preferences
latestCheckIns :: StorageT [(Name, [CheckInProof])]
latestCheckIns = do
  -- Get all users
  s <- select $ from return
  -- TODO: Get checkin preference from user profile
  -- For now use one day time span
  let ndt = fromInteger (24*60*60)

  -- Get CheckInProof for reach user
  traverse (\u -> do
        let n = userName $ entityVal u
        cs <- listCheckIns (userName $ entityVal u) (Just ndt)
        return (n, fst <$> cs)
    ) s

-- | Store a Peer
storePeer :: Host     -- ^ Peer's host
          -> Port     -- ^ Host port to connect on
          -> StorageT (Key Peer)
storePeer host port = do
  t <- liftIO getCurrentTime
  insert $ Peer host port (UTCTimeStore t)

listPeers :: StorageT [(Int64,Peer)]
listPeers = do
  s <- select $ from return
  return $ ((,) <$> fromSqlKey . entityKey <*> entityVal) <$> s

removePeer :: Int64 -> StorageT ()
removePeer i = PC.delete (toSqlKey i :: Key Peer)

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
      mM = S.openSigned <$> mPK <*> Just (toSigned c)

  case mM of
    Just mM' -> case mM' of
      Just _ -> return True
      Nothing -> return False
    Nothing -> return False

-- | Run storage actions with no logging, no pooling and silent migration
runStorage :: StorageT a -> IO a
runStorage action = dbConnectionString >>= \c -> P.runSqlite (pack c) $ do
  _ <- runMigrationSilent migrateAll
  action

-- | Run storage actions with given pool
runStoragePool :: Pool SqlBackend -> StorageT a -> IO a
runStoragePool = flip runSqlPersistMPool
