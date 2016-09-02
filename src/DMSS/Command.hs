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

data Command = Id IdCommand | Version deriving (Show, Generic)
instance Serialize Command

data IdCommand = IdCreate | IdList deriving (Show, Generic)
instance Serialize IdCommand

--data IdOptions = IdOptions
--  { test :: Bool }
--  deriving (Show, Generic)
