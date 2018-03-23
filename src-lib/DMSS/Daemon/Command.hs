-- |
-- Module      : DMSS.Command
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System Daemon command module
--

module DMSS.Daemon.Command where

import Data.Serialize
import GHC.Generics

-- | High level commands
data Command = Version | Status deriving (Show, Generic)
instance Serialize Command
