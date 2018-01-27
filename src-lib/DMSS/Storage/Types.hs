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

import           Crypto.Lithium.Unsafe.Password ( PasswordString (..) )
import           Data.ByteArray ( withByteArray )
import           Data.ByteArray.Sized
--import           Data.ByteArray.Sized ( Sized (..) )
--import           Data.ByteString
import qualified Data.Text as T
import           Database.Persist.Class
import           Database.Persist.Sql
--import           Database.Persist.Types  -- FIXME explicit imports
--import           Foreign.C.String ( newCString, peekCString )
import           Foreign.C.String ( peekCString )
import           System.IO.Unsafe ( unsafePerformIO )

newtype Name = Name { unName :: String } deriving (PersistField, PersistFieldSql)


--newtype PassHash = PassHash { unPassHash :: String } deriving (PersistField, PersistFieldSql)
newtype PassHash = PassHash { unPassHash :: PasswordString }

instance PersistField PassHash where
  toPersistValue = PersistText . T.pack . unsafePerformIO
    . flip withByteArray peekCString . unSized . unPasswordString . unPassHash

  fromPersistValue _ = undefined
  --fromPersistValue (PersistText t) = newCString . T.unpack $ t
  --fromPersistValue (PersistByteString bs) = maybe (Left "BAD") (Right . PasswordString) $ asSized bs
  --fromPersistValue (PersistByteString bs) = Right . PassHash . PasswordString . Sized $ bs

instance PersistFieldSql PassHash where
  sqlType _ = SqlString


newtype KeypairStore = KeypairStore { unKeypairStore :: String } deriving (PersistField, PersistFieldSql)
newtype CheckInProof = CheckInProof { unCheckInProof :: String }
newtype Silent = Silent { unSilent :: Bool }
