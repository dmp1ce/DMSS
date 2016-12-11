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

import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Data.Text ( pack )

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserKey
  fingerprint String
  date String
CheckIn
  raw_data String
  date String
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
  -- TODO: Get the current date
  insert $ UserKey s "Missing date"

-- | Store a CheckIn
storeCheckIn :: String -> IO (Key CheckIn)
storeCheckIn s = dbConnectionString >>= \c -> runSqlite (pack c) $ do
  -- TODO: Get the current date
  insert $ CheckIn s "Missing date"

-- | List the last `Int` checkins sorted by date 
listCheckIns :: Int -> IO ([Entity CheckIn])
listCheckIns i = dbConnectionString >>= \c -> runSqlite (pack c) $ do
  something <- selectList [] [LimitTo i]
  return (something :: [Entity CheckIn])

--runStorage :: SqlPersistT (Control.Monad.Logger.NoLoggingT (ResourceT IO)) -> IO b 
--runStorage f = dbConnectionString >>= \c -> runSqlite (pack c) f
