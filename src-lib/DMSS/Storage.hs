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
                    , PassHash (..)
                    , BoxKeypairStore (..)
                    , SignKeypairStore (..)
                    , dbConnectionString
                    , User (..)
                    , runStorage
                    , runStoragePool
                    )
  where

import           DMSS.Config ( localDirectory )
import           DMSS.Common ( getCurrentTimeInSeconds )
import           DMSS.Storage.Types
import           DMSS.Storage.TH

import qualified Database.Persist.Sqlite as P
import           Database.Esqueleto
import           Data.Text ( pack
                           , unpack
                           )

import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Logger ( NoLoggingT
                                      , runStdoutLoggingT
                                      )
import           Control.Monad.Trans.Resource ( ResourceT )

dbConnectionString :: IO String
dbConnectionString = localDirectory >>= \ld -> pure $ ld ++ "/dmss.sqlite"

-- | Store User information
storeUser :: Name           -- ^ Username
          -> PassHash       -- ^ Password Hash
          -> BoxKeypairStore   -- ^ Box keypair encrypted
          -> SignKeypairStore   -- ^ Box keypair encrypted
          -> SqlPersistT (NoLoggingT (ResourceT IO)) (Key User)
storeUser n h bkp skp = do
  t <- liftIO getCurrentTimeInSeconds
  insert $ User n h bkp skp t

removeUser :: Name  -- ^ User to delete by name
           -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
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
           -> SqlPersistT (NoLoggingT (ResourceT IO)) (Maybe (Key User))
getUserKey = getUserKeyDBActions

-- | List the last `Int` users sorted by date
listUsers :: Int -> IO [User]
listUsers i = runStorage $ do
  s <- select $
         from $ \c -> do
           limit (toEnum i)
           orderBy [desc (c ^. UserCreated)]
           return c
  return (entityVal <$> s)

-- | Underlying Database actions for getting UserKey Key
getUserKeyDBActions :: Name -> SqlPersistT (NoLoggingT (ResourceT IO)) (Maybe (Key User))
getUserKeyDBActions n = do
  maybeUser <- getBy $ UniqueName n
  maybe
    (pure Nothing)
    (\(Entity userId _) -> pure $ Just userId)
    maybeUser

-- | Store a CheckIn
storeCheckIn :: Name          -- ^ Name of users
             -> CheckInProof  -- ^ Raw checkin verification proof
             -> SqlPersistT (NoLoggingT (ResourceT IO)) (Either String (Key CheckIn))
storeCheckIn n (CheckInProof rawCheckInData) = do
  t <- liftIO $ getCurrentTimeInSeconds
  m <- getUserKeyDBActions n
  maybe (pure $ Left "Could not find users name in DB")
    (\i -> do
      res <- insert $ CheckIn i rawCheckInData t
      pure $ Right res) m

-- | List the last `Int` checkins sorted by date 
listCheckIns :: Name -> Int -> IO [Entity CheckIn]
listCheckIns n i = runStorage $ do
  s <- select $
         from $ \(u, c) -> do
           where_ ( u ^. UserName ==. val n
                &&. u ^. UserId ==. c ^. CheckInUserId
                  )
           limit (toEnum i)
           orderBy [desc (c ^. CheckInCreated)]
           return c
  return (s :: [Entity CheckIn])

-- | Run storage actions with no logging, no pooling and silent migration
runStorage :: SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runStorage action = dbConnectionString >>= \c -> P.runSqlite (pack c) $ do
  _ <- runMigrationSilent migrateAll
  action

-- | Run storage actions with stdout logging, pooling and stdout migration
runStoragePool :: SqlPersistT (NoLoggingT (ResourceT IO)) a -> IO a
runStoragePool action = dbConnectionString >>= \c -> runStdoutLoggingT $ P.withSqlitePool (pack c) 10 $ \pool -> liftIO $ do
  flip runSqlPersistMPool pool $ do
    stuff <- runMigrationSilent migrateAll
    liftIO $ mapM_ (putStrLn . unpack) stuff
    action
