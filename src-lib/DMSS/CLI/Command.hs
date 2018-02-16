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

-- | High level commands
data Command = Id IdCommand
             | CheckIn CheckInCommand
             | Version
             | Status deriving (Show)

-- | Id command
data IdCommand = IdCreate (Maybe String) (Maybe String) -- ^ Name of id and contact information
               | IdRemove String                      -- ^ Fingerprint
               | IdList deriving (Show)

-- | CheckIn command
data CheckInCommand = CheckInCreate String -- ^ Create new checkin
                    | CheckInList            -- ^ List past checkins
  deriving (Show)
