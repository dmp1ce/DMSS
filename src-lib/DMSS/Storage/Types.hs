-- |
-- Module      : DMSS.Storage.Types
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System storage types
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DMSS.Storage.Types where

import           Database.Persist.Class
import           Database.Persist.Sql

newtype Name = Name { unName :: String } deriving (PersistField, PersistFieldSql)
newtype PassHash = PassHash { unPassHash :: String } deriving (PersistField, PersistFieldSql)
newtype KeypairStore = KeypairStore { unKeypairStore :: String } deriving (PersistField, PersistFieldSql)
newtype CheckInProof = CheckInProof { unCheckInProof :: String }
newtype Silent = Silent { unSilent :: Bool }
