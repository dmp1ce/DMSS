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
{-# LANGUAGE OverloadedStrings #-}

module DMSS.Storage.Types where

import Crypto.Lithium.Password ( storePassword, sensitivePolicy )
import Crypto.Lithium.Types ( Sized (unSized), fromPlaintext, toPlaintext )
import Crypto.Lithium.Unsafe.Password ( PasswordString (..) )
import Data.ByteString ( ByteString )
import Data.String (fromString)
import Data.String.Conv ( toS )
import Data.Text ( Text, append )
import Database.Persist ( PersistField, fromPersistValue, toPersistValue )
import Database.Persist.Sql ( PersistFieldSql, sqlType )
import Database.Persist.Types
   ( PersistValue (PersistList)
   , SqlType (SqlString)
   )
import qualified Data.ByteString.Base64 as B64
import System.IO.Unsafe ( unsafePerformIO )

-- For testing
import Test.QuickCheck
   ( Arbitrary (..)
   , arbitrary
--   , Gen
--   , listOf
--   , elements
   )


newtype Name = Name { unName :: String } deriving (Show, PersistField, PersistFieldSql)


newtype PassHash = PassHash { unPassHash :: ByteString }
   deriving (Eq, PersistField, PersistFieldSql, Show)


{- | Hash a password into a PassHash

   This type is our internal representation of a hashed password, so we may
   store it easily.
-}
hashPassword :: String -> PassHash
hashPassword = toPassHash . unsafePerformIO . storePassword sensitivePolicy
  . fromString


{- | Convert a PasswordString to a PassHash

   PasswordString is the internal representation of a hashed password for the
   lithium library. This function converts one to our data structure.
-}
toPassHash :: PasswordString -> PassHash
toPassHash = PassHash . fromPlaintext . unSized . unPasswordString


{- | Convert a PassHash to a PasswordString

   PasswordString is the internal representation of a hashed password for the
   lithium library. This function converts our PassHash type to it.
-}
fromPassHash :: PassHash -> Either Text PasswordString
fromPassHash (PassHash bs) = maybe
  (Left . append "Reading password hash from database failed because we cannot parse this data: " . toS $ bs)
  (Right . PasswordString)
  $ toPlaintext bs


newtype Password = Password String deriving Show


data BoxKeypairStore = BoxKeypairStore { boxSecretKeyStore :: ByteString
                                       , boxPublicKeyStore :: ByteString
                                       } deriving (Show, Eq)

instance PersistField BoxKeypairStore where
  toPersistValue (BoxKeypairStore sk pk) =
    PersistList [toPersistValue sk, toPersistValue pk]
  fromPersistValue v = case fromPersistValue v of
    Right (sk:pk:[]) -> BoxKeypairStore <$> fromPersistValue sk
                                        <*> fromPersistValue pk
    Left t  -> Left . toS $ show t
    Right r -> Left . toS $ "Did not recieve the expected two values."
                             ++ show r
instance PersistFieldSql BoxKeypairStore where
  sqlType _ = SqlString


data SignKeypairStore = SignKeypairStore { signSecretKeyStore :: ByteString
                                       , signPublicKeyStore :: ByteString
                                       } deriving (Show, Eq)

instance PersistField SignKeypairStore where
  toPersistValue (SignKeypairStore sk pk) =
    PersistList [toPersistValue sk, toPersistValue pk]
  fromPersistValue v = case fromPersistValue v of
    Right (sk:pk:[]) -> SignKeypairStore <$> fromPersistValue sk
                                         <*> fromPersistValue pk
    Left t  -> Left . toS $ show t
    Right r -> Left . toS $ "Did not recieve the expected two values."
                             ++ show r
instance PersistFieldSql SignKeypairStore where
  sqlType _ = SqlString


newtype CheckInProof = CheckInProof { unCheckInProof :: String }


newtype Silent = Silent { unSilent :: Bool }


-- For testing

-- KeypairStores should be base64 encoded to prevent characters with special functions
instance Arbitrary BoxKeypairStore where
  arbitrary = do
    s <- arbitrary
    p <- arbitrary
    return $ BoxKeypairStore ((B64.encode . toS) (s::String))
                             ((B64.encode . toS) (p::String))
instance Arbitrary SignKeypairStore where
  arbitrary = do
    s <- arbitrary
    p <- arbitrary
    return $ SignKeypairStore ((B64.encode . toS) (s::String))
                              ((B64.encode . toS) (p::String))
