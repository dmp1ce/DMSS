-- |
-- Module      : DMSS.Common
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System common functions
--

module DMSS.Common ( getCurrentTimeInSeconds
                   , verifyCheckIn
                   ) where

import DMSS.Storage.Types

import Database.Persist.Sqlite
import Data.Time.Clock
import Data.Time.Clock.POSIX

getCurrentTimeInSeconds :: IO Int
getCurrentTimeInSeconds = getCurrentTime >>= \t -> pure $ fromEnum $ utcTimeToPOSIXSeconds t

verifyCheckIn :: Entity CheckIn -> Bool
verifyCheckIn _ = False
