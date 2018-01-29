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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Data.Either

newtype Name = Name { unName :: String } deriving (Show, PersistField, PersistFieldSql)
newtype PassHash = PassHash { unPassHash :: String } deriving (Show, PersistField, PersistFieldSql)
newtype Password = Password String deriving Show

data BoxKeypairStore = BoxKeypairStore { boxSecretKeyStore :: BS.ByteString
                                       , boxPublicKeyStore :: BS.ByteString
                                       } deriving (Show)
instance PersistField BoxKeypairStore where
  toPersistValue (BoxKeypairStore sk _) = PersistList [toPersistValue sk, toPersistValue sk]
  fromPersistValue v = case fromPersistValue v of
    Right (sk:pk:[]) -> Right $ BoxKeypairStore (BS8.pack $ fromRight "" $ fromPersistValue sk) (BS8.pack $ fromRight "" $ fromPersistValue pk)
    Left t -> Left $ T.pack $ "Busted: " ++ show t
    _      -> Left $ T.pack "Busted!"
instance PersistFieldSql BoxKeypairStore where
  sqlType _ = SqlString

newtype CheckInProof = CheckInProof { unCheckInProof :: String }
newtype Silent = Silent { unSilent :: Bool }
