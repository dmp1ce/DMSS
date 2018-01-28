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
import           Crypto.Lithium.Types ( Sized (unSized), fromPlaintext, toPlaintext )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import           Database.Persist ( PersistField, fromPersistValue, toPersistValue )
import           Database.Persist.Sql ( PersistFieldSql, sqlType )
import           Database.Persist.Types ( PersistValue (PersistByteString), SqlType (SqlString) )

newtype Name = Name { unName :: String } deriving (PersistField, PersistFieldSql)


newtype PassHash = PassHash { unPassHash :: PasswordString }

instance PersistField PassHash where
  --toPersistValue :: PassHash -> PersistValue
  toPersistValue = PersistByteString . fromPlaintext . unSized . unPasswordString . unPassHash

  --fromPersistValue :: PersistValue -> Either Text PassHash
  fromPersistValue (PersistByteString bs) = maybe
    (deserializePassHashErrorMsg . T.decodeUtf8With T.strictDecode $ bs)
    (Right . PassHash . PasswordString)
    $ toPlaintext bs
  fromPersistValue perVal= deserializePassHashErrorMsg . T.pack . show $ perVal

deserializePassHashErrorMsg :: T.Text -> Either T.Text a
deserializePassHashErrorMsg = Left . T.append (T.pack
  "Deserialization of password hash from database failed because we cannot parse this data: ")

instance PersistFieldSql PassHash where
  sqlType _ = SqlString


newtype KeypairStore = KeypairStore { unKeypairStore :: String } deriving (PersistField, PersistFieldSql)
newtype CheckInProof = CheckInProof { unCheckInProof :: String }
newtype Silent = Silent { unSilent :: Bool }
