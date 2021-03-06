-- |
-- Module      : DMSS.Storage.TH
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System storage schema
--

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DMSS.Storage.TH where

import           Database.Persist.TH
import           DMSS.Storage.Types
import qualified Data.ByteString.Char8  as BS8

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Name                   -- ^ User's name
  hashSalt HashSalt           -- ^ Password hash and salt storage
  boxKeypairStore  BoxKeypairStore    -- ^ encrypted box keypair
  signKeypairStore  SignKeypairStore  -- ^ encrypted sign keypair
  UniqueName name
  created UTCTimeStore        -- ^ Creation time
  deriving Show
CheckIn
  userId UserId
  raw_data BS8.ByteString
  created UTCTimeStore        -- ^ Creation time
  deriving Show
Peer
  host Host                   -- ^ Host of peer
  port Port                   -- ^ Port to connect on
  created UTCTimeStore        -- ^ Creation time
  deriving Show
|]
