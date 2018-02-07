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

import DMSS.Storage.TH
import DMSS.Storage.Types
--import DMSS.Config

import Database.Persist.Sqlite
import Data.Time.Clock
import Data.Time.Clock.POSIX
--import qualified Data.ByteString.Char8   as C

getCurrentTimeInSeconds :: IO Int
getCurrentTimeInSeconds = getCurrentTime >>= \t -> pure $ fromEnum $ utcTimeToPOSIXSeconds t

verifyCheckIn :: Name -> DMSS.Storage.Types.Password -> Entity CheckIn -> IO Bool
verifyCheckIn (Name _) (DMSS.Storage.Types.Password _) _ = return False
