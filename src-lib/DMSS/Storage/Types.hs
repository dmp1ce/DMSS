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

import qualified Crypto.Lithium.Password as LP
import Crypto.Lithium.Types ( fromPlaintext, toPlaintext, BytesN )
import Data.ByteString ( ByteString )
import Data.String.Conv ( toS )
import Data.Text ( Text, append )
import Database.Persist ( PersistField, fromPersistValue, toPersistValue )
import Database.Persist.Sql ( PersistFieldSql, sqlType )
import Database.Persist.Types ( PersistValue (PersistList), SqlType (SqlString) )
import qualified Data.ByteString.Base64 as B64

-- For testing
import           Test.QuickCheck  ( Arbitrary (..)
                                  , arbitrary
--                                  , Gen
--                                  , listOf
--                                  , elements
                                  )


newtype Name = Name { unName :: String } deriving (Show, PersistField, PersistFieldSql)


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

deserializePassHashErrorMsg :: Text -> Either Text a
deserializePassHashErrorMsg = Left . append (toS
  "Reading password hash from database failed because we cannot parse this data: ")

instance PersistFieldSql PassHash where
  sqlType _ = SqlString


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
