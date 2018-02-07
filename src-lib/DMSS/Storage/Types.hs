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

import qualified Crypto.Lithium.Password as LP
import Crypto.Lithium.Types ( fromPlaintext, toPlaintext )
import Data.ByteString ( ByteString )
import Data.String.Conv ( toS )
import Data.Text ( Text, append )
import Database.Persist ( PersistField, fromPersistValue, toPersistValue )
import Database.Persist.Sql ( PersistFieldSql, sqlType )
import Database.Persist.Types ( PersistValue (PersistList), SqlType (SqlString) )
import qualified Data.ByteString.Base64 as B64

-- For testing
import Test.QuickCheck
   ( Arbitrary (..)
   , arbitrary
--   , Gen
--   , listOf
--   , elements
   )


newtype Name = Name { unName :: String } deriving (Show, PersistField, PersistFieldSql)

-- | Represents password hash and salt to decrypt keypairs
data HashSalt = HashSalt ByteString ByteString deriving (Eq, Show)

instance PersistField HashSalt where
  toPersistValue (HashSalt p s) =
    PersistList [toPersistValue p, toPersistValue s]
  fromPersistValue v = case fromPersistValue v of
    Right (p:s:[]) -> HashSalt <$> fromPersistValue p <*> fromPersistValue s
    Left t  -> Left . toS $ show t
    Right r -> Left . toS $ "Did not recieve the expected two values."
                             ++ show r
instance PersistFieldSql HashSalt where
  sqlType _ = SqlString

{- | Convert a `PasswordString` and `Salt` to a `HashSalt`

   `PasswordString` a Lithium hashed password.
   `Salt` is a Lithium salt for deriving a keypair.
-}
toHashSalt :: LP.PasswordString -> LP.Salt -> HashSalt
toHashSalt p s = HashSalt
  ((fromPlaintext $ LP.unPasswordString p) :: ByteString)
  ((fromPlaintext $ LP.unSalt s) :: ByteString)

{- | Convert a `PassHash` to a (`PasswordString`,`Salt`)

   `PasswordString` a Lithium hashed password.
   `Salt` is a Lithium salt for deriving a keypair.
-}
fromHashSalt :: HashSalt -> Either Text (LP.PasswordString, LP.Salt)
fromHashSalt (HashSalt p s) = (,) <$> eP <*> eS
  where
    eP = maybe
      (Left . append "Reading password hash from database failed because we cannot parse this data: " . toS $ p)
      (Right . LP.PasswordString) $ toPlaintext p
    eS = maybe
      (Left . append "Reading salt from database failed because we cannot parse this data: " . toS $ s)
      (Right . LP.Salt) $ toPlaintext s

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

newtype CheckInProof = CheckInProof ByteString
mkCheckInProof :: ByteString -> CheckInProof
mkCheckInProof = CheckInProof . B64.encode
unCheckInProof :: CheckInProof -> ByteString
unCheckInProof (CheckInProof p) = either (const p) id (B64.decode p)

newtype Silent = Silent { unSilent :: Bool }


-- For testing

-- Should use base64 encoded to prevent characters with special functions
instance Arbitrary HashSalt where
  arbitrary = do
    p <- arbitrary
    s <- arbitrary
    return $ HashSalt ((B64.encode . toS) (p::String))
                      ((B64.encode . toS) (s::String))

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
