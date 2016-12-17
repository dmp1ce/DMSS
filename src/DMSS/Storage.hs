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
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module DMSS.Storage ( storeCheckIn
                    , listCheckIns
                    , migrateStorage
                    , storeUserKey
                    , getUserKeyKey
                    , removeUserKey
                    , CheckInId
                    , UserKeyId
                    , Fingerprint (..)
                    , CheckInProof (..)
                    )
  where


import           DMSS.Config ( localDirectory )
import           DMSS.Util  ( getCurrentTimeInSeconds )

import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Text ( pack
                           , Text
                           )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserKey
  fingerprint String -- ^ Fingerprint of GPG key
  UniqueFingerprint fingerprint
  created Int -- ^ POSIX time
CheckIn
  userId UserKeyId
  raw_data String
  created Int -- ^ POSIX time
  deriving Show
|]

newtype Fingerprint  = Fingerprint  { unFingerprint :: String }
newtype CheckInProof = CheckInProof { unCheckInProof :: String }

dbConnectionString :: IO String
dbConnectionString = localDirectory >>= (\ld -> pure $ ld ++ "/dmss.sqlite")

-- | Run Persistent migration
migrateStorage :: IO [Text]
migrateStorage = do
  c <- dbConnectionString
  runSqlite (pack c) $ runMigrationSilent migrateAll

-- | Store UserKey information
storeUserKey :: Fingerprint -- ^ UserKey Fingerprint
             -> IO (Key UserKey)
storeUserKey (Fingerprint s) = do
  t <- getCurrentTimeInSeconds
  dbConnectionString >>= \c -> runSqlite (pack c) $ insert $ UserKey s t

removeUserKey :: Fingerprint -- ^ UserKey Fingerprint
              -> IO ()
removeUserKey (Fingerprint fpr) = do
  dbConnectionString >>= \c -> runSqlite (pack c) $ do
    -- Delete all checkins associated with UserKey
    mUserKey <- getBy $ UniqueFingerprint fpr
    case mUserKey of
      Nothing  -> error "Couldn't find UserKey"
      (Just uk) -> deleteWhere [ UserKeyId ==. (entityKey uk) ]
    -- Then delete UserKey
    deleteBy $ UniqueFingerprint fpr

-- | Get UserKey ID
getUserKeyKey :: Fingerprint -- ^ UserKey Fingerprint
             -> IO (Maybe (Key UserKey))
getUserKeyKey (Fingerprint fpr) = dbConnectionString >>= \c -> runSqlite (pack c) $ do
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
  m <- getUserKeyKey fpr
  dbConnectionString >>= \c -> runSqlite (pack c) $ maybe
    (pure $ Left "Could not find users fingerprint in DB")
    (\i -> do
      res <- insert $ CheckIn i rawCheckInData t
      pure $ Right res) m

-- | List the last `Int` checkins sorted by date 
listCheckIns :: Int -> IO ([Entity CheckIn])
listCheckIns i = dbConnectionString >>= \c -> runSqlite (pack c) $ do
  s <- selectList [] [LimitTo i]
  return (s :: [Entity CheckIn])

--runStorage :: SqlPersistT (Control.Monad.Logger.NoLoggingT (ResourceT IO)) -> IO b 
--runStorage f = dbConnectionString >>= \c -> runSqlite (pack c) f
