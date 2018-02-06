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
import Crypto.Lithium.Types ( fromPlaintext, toPlaintext, BytesN )
--import Crypto.Lithium.Password ( storePassword, sensitivePolicy )
import Data.ByteString ( ByteString )
--import Data.String (fromString)
import Data.String.Conv ( toS )
import Data.Text ( Text )
import Database.Persist ( PersistField, fromPersistValue, toPersistValue )
import Database.Persist.Sql ( PersistFieldSql, sqlType )
import Database.Persist.Types ( PersistValue (PersistList), SqlType (SqlString) )
import qualified Data.ByteString.Base64 as B64
--import System.IO.Unsafe ( unsafePerformIO )

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
data PassHash = PassHash { unPassHash :: LP.PasswordString
                         , unSalt :: LP.Salt } deriving Show

instance PersistField PassHash where
  toPersistValue :: PassHash -> PersistValue
  toPersistValue (PassHash p s) =
    PersistList [ toPersistValue $ ((fromPlaintext $ LP.unPasswordString p) :: ByteString)
                , toPersistValue $ ((fromPlaintext $ LP.unSalt s) :: ByteString) ]

  fromPersistValue :: PersistValue -> Either Text PassHash
  fromPersistValue v = case fromPersistValue v of
    Right (p:s:[]) ->
      let eP = fromPersistValue p :: Either Text ByteString
          eS = fromPersistValue s :: Either Text ByteString
          ePS = (,) <$> eP <*> eS
      in case ePS of
        Right (p',s') ->
          let mP = toPlaintext p' :: Maybe (BytesN LP.PasswordStringBytes)
              mS = toPlaintext s' :: Maybe (BytesN LP.SaltBytes)
              mPS = (,) <$> mP <*> mS :: Maybe (BytesN LP.PasswordStringBytes, BytesN LP.SaltBytes)
          in case mPS of
            Just (p'',s'') -> Right $ PassHash (LP.PasswordString p'')  (LP.Salt s'')
            Nothing -> undefined
        Left t  -> Left . toS $ show t
        
    Left t  -> Left . toS $ show t
    Right r -> Left . toS $ "Did not recieve the expected two values."
                             ++ show r
  --fromPersistValue (PersistByteString bs) = maybe
  --  (deserializePassHashErrorMsg . toS $ bs)
  --  (Right . PassHash . PasswordString)
  --  $ toPlaintext bs
  --fromPersistValue perVal= deserializePassHashErrorMsg . toS . show $ perVal
instance PersistFieldSql PassHash where
  sqlType _ = SqlString


{- | Hash a password into a PassHash
   This type is our internal representation of a hashed password, so we may
   store it easily.
-}
--hashPassword :: String -> PassHash
--hashPassword = undefined
--toPassHash . unsafePerformIO . storePassword sensitivePolicy
--  . fromString


{- | Convert a PasswordString to a PassHash

   PasswordString is the internal representation of a hashed password for the
   lithium library. This function converts one to our data structure.
-}
--toPassHash :: PasswordString -> PassHash
--toPassHash = undefined --PassHash . fromPlaintext . unSized . unPasswordString


{- | Convert a PassHash to a PasswordString

   PasswordString is the internal representation of a hashed password for the
   lithium library. This function converts our PassHash type to it.
-}
--fromPassHash :: PassHash -> Either Text PasswordString
--fromPassHash (PassHash bs) = undefined
--maybe
--  (Left . append "Reading password hash from database failed because we cannot parse this data: " . toS $ bs)
--  (Right . PasswordString)
--  $ toPlaintext bs

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

-- KeypairStores should be base64 encoded to prevent characters with special functions
--instance Arbitrary PassHash where
--  arbitrary = do
--    pass <- arbitrary
--    salt <- arbitrary
--    return $ PassHash $ ((B64.encode . toS) (pass::String))
--                        ((B64.encode . toS) (salt::String))

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
