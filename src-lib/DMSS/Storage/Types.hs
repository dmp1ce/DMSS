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
{-# LANGUAGE InstanceSigs #-}

module DMSS.Storage.Types where

import Crypto.Lithium.Unsafe.Password ( PasswordString (..) )
import Crypto.Lithium.Types ( Sized (unSized), fromPlaintext, toPlaintext )
import Data.ByteString ( ByteString )
import Data.Either ( fromRight )
import Data.String.Conv ( toS )
import Data.Text ( Text, append )
import Database.Persist ( PersistField, fromPersistValue, toPersistValue )
import Database.Persist.Sql ( PersistFieldSql, sqlType )
import Database.Persist.Types ( PersistValue (PersistList, PersistByteString), SqlType (SqlString) )


newtype Name = Name { unName :: String } deriving (Show, PersistField, PersistFieldSql)


newtype PassHash = PassHash { unPassHash :: PasswordString }

instance PersistField PassHash where
  toPersistValue :: PassHash -> PersistValue
  toPersistValue = PersistByteString . fromPlaintext . unSized . unPasswordString . unPassHash

  fromPersistValue :: PersistValue -> Either Text PassHash
  fromPersistValue (PersistByteString bs) = maybe
    (deserializePassHashErrorMsg . toS $ bs)
    (Right . PassHash . PasswordString)
    $ toPlaintext bs
  fromPersistValue perVal= deserializePassHashErrorMsg . toS . show $ perVal

deserializePassHashErrorMsg :: Text -> Either Text a
deserializePassHashErrorMsg = Left . append (toS
  "Reading password hash from database failed because we cannot parse this data: ")

instance PersistFieldSql PassHash where
  sqlType _ = SqlString


newtype Password = Password String deriving Show


data BoxKeypairStore = BoxKeypairStore { boxSecretKeyStore :: ByteString
                                       , boxPublicKeyStore :: ByteString
                                       } deriving (Show)

instance PersistField BoxKeypairStore where
  toPersistValue (BoxKeypairStore sk _) = PersistList [toPersistValue sk, toPersistValue sk]

  fromPersistValue v = case fromPersistValue v of
    Right (sk:pk:[]) -> Right $ BoxKeypairStore (toS . fromRight "" . fromPersistValue $ sk) (toS . fromRight "" . fromPersistValue $ pk)
    Left t -> Left . toS $ "Busted: " ++ show t
    _      -> Left . toS $ "Busted!"

instance PersistFieldSql BoxKeypairStore where
  sqlType _ = SqlString


newtype CheckInProof = CheckInProof { unCheckInProof :: String }

newtype Silent = Silent { unSilent :: Bool }
