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
                    , CheckInId
                    , UserKeyId
                    )
  where


import           DMSS.Config ( localDirectory )
import           DMSS.Util  ( getCurrentTimeInSeconds )

import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Text ( pack )

import           Control.Monad.IO.Class ( liftIO )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserKey
  fingerprint String
  date Int -- ^ POSIX time
CheckIn
  raw_data String
  date Int -- ^ POSIX time
  deriving Show
|]

dbConnectionString :: IO String
dbConnectionString = localDirectory >>= (\ld -> pure $ ld ++ "/dmss.sqlite")

-- | Run Persistent migration
migrateStorage :: IO ()
migrateStorage = do
  c <- dbConnectionString
  runSqlite (pack c) $ runMigration migrateAll

-- | Store UsedKey information
storeUserKey :: String -> IO (Key UserKey)
storeUserKey s = dbConnectionString >>= \c -> runSqlite (pack c) $ do
  t <- liftIO getCurrentTimeInSeconds
  insert $ UserKey s t

-- | Store a CheckIn
storeCheckIn :: String -> IO (Key CheckIn)
storeCheckIn s = dbConnectionString >>= \c -> runSqlite (pack c) $ do
  t <- liftIO getCurrentTimeInSeconds
  insert $ CheckIn s t

-- | List the last `Int` checkins sorted by date 
listCheckIns :: Int -> IO ([Entity CheckIn])
listCheckIns i = dbConnectionString >>= \c -> runSqlite (pack c) $ do
  something <- selectList [] [LimitTo i]
  return (something :: [Entity CheckIn])

--runStorage :: SqlPersistT (Control.Monad.Logger.NoLoggingT (ResourceT IO)) -> IO b 
--runStorage f = dbConnectionString >>= \c -> runSqlite (pack c) f
