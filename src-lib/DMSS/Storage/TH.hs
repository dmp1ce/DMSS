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
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module DMSS.Storage.TH where

import           Database.Persist.TH
import           DMSS.Storage.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  name Name                   -- ^ User's name
  passwordStore PassHash      -- ^ Password storage hash
  boxKeypairStore  BoxKeypairStore  -- ^ Keypair stored as seed
  signKeypairStore  SignKeypairStore  -- ^ Keypair stored as seed
  UniqueName name
  created Int                 -- ^ POSIX time
  deriving Show
CheckIn
  userId UserId
  raw_data String
  created Int                 -- ^ POSIX time
  deriving Show
|]
