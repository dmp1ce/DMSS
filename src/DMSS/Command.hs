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

data Command = Id IdCommand
             | Version
             | Status deriving (Show, Generic)
instance Serialize Command

-- | Top level commands
data IdCommand = IdCreate (Maybe String) -- Name of id
                          (Maybe String) -- ^ Contact information
               | IdRemove
               | IdList deriving (Show, Generic)
instance Serialize IdCommand
