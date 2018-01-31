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
import qualified Data.ByteString.Char8  as BS8
import qualified Data.Text              as T

-- For testing
import           Test.QuickCheck  ( Arbitrary (..)
                                  , arbitrary
                                  , Gen
                                  , listOf
                                  , elements
                                  )


newtype Name = Name { unName :: String } deriving (Show, PersistField, PersistFieldSql)
newtype PassHash = PassHash { unPassHash :: String } deriving (Show, PersistField, PersistFieldSql)
newtype Password = Password String deriving Show

data BoxKeypairStore = BoxKeypairStore { boxSecretKeyStore :: BS8.ByteString
                                       , boxPublicKeyStore :: BS8.ByteString
                                       } deriving (Show, Eq)
instance PersistField BoxKeypairStore where
  toPersistValue (BoxKeypairStore sk pk) =
    PersistList [toPersistValue sk, toPersistValue pk]
  fromPersistValue v = case fromPersistValue v of
    Right (sk:pk:[]) -> BoxKeypairStore <$> fromPersistValue sk
                                        <*> fromPersistValue pk
    Left t  -> Left $ T.pack $ show t
    Right r -> Left $ T.pack $ "Did not recieve the expected two values."
                             ++ show r
instance PersistFieldSql BoxKeypairStore where
  sqlType _ = SqlString

data SignKeypairStore = SignKeypairStore { signSecretKeyStore :: BS8.ByteString
                                       , signPublicKeyStore :: BS8.ByteString
                                       } deriving (Show, Eq)
instance PersistField SignKeypairStore where
  toPersistValue (SignKeypairStore sk pk) =
    PersistList [toPersistValue sk, toPersistValue pk]
  fromPersistValue v = case fromPersistValue v of
    Right (sk:pk:[]) -> SignKeypairStore <$> fromPersistValue sk
                                         <*> fromPersistValue pk
    Left t  -> Left $ T.pack $ show t
    Right r -> Left $ T.pack $ "Did not recieve the expected two values."
                             ++ show r
instance PersistFieldSql SignKeypairStore where
  sqlType _ = SqlString

newtype CheckInProof = CheckInProof { unCheckInProof :: String }
newtype Silent = Silent { unSilent :: Bool }

-- For testing

-- Generate some 'safe' strings
-- https://stackoverflow.com/a/20936497/350221
genSafeChar :: Gen Char
genSafeChar = elements (['a'..'z'] ++ ['0'..'9'] ++
  ['$','\\','#','/','%','@','!'])
genSafeString :: Gen String
genSafeString = listOf genSafeChar

-- KeypairStores should never be a non visible characters
instance Arbitrary BoxKeypairStore where
  arbitrary = do
    s <- genSafeString
    p <- genSafeString
    return $ BoxKeypairStore (BS8.pack s) (BS8.pack p)
instance Arbitrary SignKeypairStore where
  arbitrary = do
    s <- genSafeString
    p <- genSafeString
    return $ SignKeypairStore (BS8.pack s) (BS8.pack p)
