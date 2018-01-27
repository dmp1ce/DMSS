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
import           Data.ByteString (ByteString)
import qualified Data.Text as T

newtype Name = Name { unName :: String } deriving (PersistField, PersistFieldSql)
newtype PassHash = PassHash { unPassHash :: String } deriving (PersistField, PersistFieldSql)

data BoxKeypairStore = BoxKeypairStore { boxSecretKeyStore :: ByteString
                                       , boxPublicKeyStore :: String
                                       }
instance PersistField BoxKeypairStore where
  toPersistValue (BoxKeypairStore sk pk) = toPersistValue (sk, pk)
  fromPersistValue (PersistList ((PersistByteString sk):(PersistText pk):[])) = Right $ BoxKeypairStore sk (T.unpack pk)
  fromPersistValue _ = Left $ T.pack "Busted"
instance PersistFieldSql BoxKeypairStore where
  sqlType _ = SqlString

newtype CheckInProof = CheckInProof { unCheckInProof :: String }
newtype Silent = Silent { unSilent :: Bool }
