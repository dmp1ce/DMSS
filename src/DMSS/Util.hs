-- |
-- Module      : DMSS.Util
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System storage module
--

module DMSS.Util ( getCurrentTimeInSeconds ) where

import Data.Time.Clock
import Data.Time.Clock.POSIX

getCurrentTimeInSeconds :: IO Int
getCurrentTimeInSeconds = getCurrentTime >>= \t -> pure $ fromEnum $ utcTimeToPOSIXSeconds t
