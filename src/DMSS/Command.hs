-- |
-- Module      : DMSS.Command
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System command module
--

{-# LANGUAGE DeriveGeneric #-}
module DMSS.Command where

import Data.Serialize
import GHC.Generics

-- | High level commands
data Command = Id IdCommand
             | Version
             | Status deriving (Show, Generic)
instance Serialize Command

-- | Id command
data IdCommand = IdCreate (Maybe String) (Maybe String) -- ^ Name of id and contact information
               | IdRemove (String)                      -- ^ Fingerprint
               | IdList deriving (Show, Generic)
instance Serialize IdCommand
