-- |
-- Module      : DMSS.BlockTime.Types
-- License     : Public Domain
--
-- Maintainer  : dino@ui3.info
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System: Types for BlockTime API
--

{- # LANGUAGE OverloadedStrings #-}

module DMSS.BlockTime.Types
where

import Control.Exception.Safe ( Exception, Typeable )
import Data.Text ( Text )
import Data.Time ( UTCTime )


data BlockexplorerException = BlockexplorerException
  deriving Typeable

instance Show BlockexplorerException where
  show BlockexplorerException =
    "Could not get block info from block explorer service"

instance Exception BlockexplorerException


data BTCBlock = BTCBlock
  { blHash :: Text
  , blTime :: UTCTime
  }
  deriving Show
