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
                    , storeUserKey
                    , getUserKeyKey
                    , removeUserKey
                    , CheckInId
                    , UserKeyId
                    , Fingerprint (..)
                    , CheckInProof (..)
                    , dbConnectionString
                    )
  where

import           DMSS.Config ( localDirectory )
import           DMSS.Common ( getCurrentTimeInSeconds )
import           DMSS.Storage.Types

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

-- | Store UserKey information
storeUserKey :: Fingerprint -- ^ UserKey Fingerprint
             -> IO (Key UserKey)
storeUserKey (Fingerprint s) = do
  t <- getCurrentTimeInSeconds
  runStorage $ insert $ UserKey s t

removeUserKey :: Fingerprint -- ^ UserKey Fingerprint
              -> IO ()
removeUserKey (Fingerprint fpr) = do
  runStorage $ do
    -- Delete all checkins associated with UserKey
    mUserKey <- getBy $ UniqueFingerprint fpr
    case mUserKey of
      Nothing  -> error "Couldn't find UserKey"
      (Just uk) -> P.deleteWhere [ UserKeyId P.==. (entityKey uk) ]
    -- Then delete UserKey
    deleteBy $ UniqueFingerprint fpr

-- | Get UserKey ID
getUserKeyKey :: Silent      -- ^ is silent and no pool?
              -> Fingerprint -- ^ UserKey Fingerprint
              -> IO (Maybe (Key UserKey))
getUserKeyKey (Silent True) fpr = runStorage $ getUserKeyKeyDBActions fpr
getUserKeyKey (Silent False) fpr = runStoragePool $ getUserKeyKeyDBActions fpr

-- | Underlying Database actions for getting UserKey Key
getUserKeyKeyDBActions :: Fingerprint -> SqlPersistT (NoLoggingT (ResourceT IO)) (Maybe (Key UserKey))
getUserKeyKeyDBActions (Fingerprint fpr) = do
  maybeUserKey <- getBy $ UniqueFingerprint fpr
  maybe
    (pure Nothing)
    (\(Entity userKeyId _) -> pure $ Just userKeyId)
    maybeUserKey

-- | Store a CheckIn
storeCheckIn :: Fingerprint   -- ^ Fingerprint of users key
             -> CheckInProof  -- ^ Raw checkin verification proof
             -> IO (Either String (Key CheckIn))
storeCheckIn fpr (CheckInProof rawCheckInData) = do
  t <- getCurrentTimeInSeconds
  runStorage $ do
    m <- getUserKeyKeyDBActions fpr
    maybe (pure $ Left "Could not find users fingerprint in DB")
      (\i -> do
        res <- insert $ CheckIn i rawCheckInData t
        pure $ Right res) m

-- | List the last `Int` checkins sorted by date 
listCheckIns :: Fingerprint -> Int -> IO [Entity CheckIn]
listCheckIns (Fingerprint fpr) i = runStorage $ do
  s <- select $
         from $ \(u, c) -> do
           where_ ( u ^. UserKeyFingerprint ==. val fpr
                &&. u ^. UserKeyId ==. c ^. CheckInUserId
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
