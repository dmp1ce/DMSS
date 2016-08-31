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

-- IdOptions
data Command = Id | Version deriving (Show, Generic)

instance Serialize Command
