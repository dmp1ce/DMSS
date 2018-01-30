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
import           Data.Either

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
  toPersistValue (BoxKeypairStore sk pk) = PersistList [toPersistValue sk, toPersistValue pk]
  fromPersistValue v = case fromPersistValue v of
    Right (sk:pk:[]) -> Right $ BoxKeypairStore (BS8.pack $ fromRight "" $ fromPersistValue sk) (BS8.pack $ fromRight "" $ fromPersistValue pk)
    Left t -> Left $ T.pack $ "Busted: " ++ show t
    _      -> Left $ T.pack "Busted!"
instance PersistFieldSql BoxKeypairStore where
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

-- BoxKeypairStore should never be a non visible charater,
-- so don't genereate those
instance Arbitrary BoxKeypairStore where
  arbitrary = do
    s <- genSafeString
    p <- genSafeString
    return $ BoxKeypairStore (BS8.pack s) (BS8.pack p)
