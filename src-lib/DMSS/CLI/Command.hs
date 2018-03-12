-- |
-- Module      : DMSS.CLI.Command
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System CLI command module
--
module DMSS.CLI.Command where

import Network (PortNumber)
import Data.Int (Int64)

-- | High level commands
data Command = Id IdCommand
             | CheckIn CheckInCommand
             | Peer PeerCommand
             | Version
             | Status deriving (Show)

-- | Id command
data IdCommand = IdCreate (Maybe String) (Maybe String) -- ^ Name of id and contact information
               | IdRemove String                      -- ^ Fingerprint
               | IdList deriving (Show)

-- | CheckIn command
data CheckInCommand = CheckInCreate String  -- ^ Create new checkin
                    | CheckInList           -- ^ List past checkins
  deriving (Show)

-- | Peer command
data PeerCommand = PeerCreate String PortNumber  -- ^ Create new peer
                 | PeerList                      -- ^ List peer
                 | PeerRemove Int64
  deriving (Show)
