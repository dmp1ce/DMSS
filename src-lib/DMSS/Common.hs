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
                   , toUTCTime, isoFormatCurrentUTCTime
                   ) where

import Data.Time.Clock
import Data.Time.Clock.POSIX ( getCurrentTime
                             , posixSecondsToUTCTime
                             , utcTimeToPOSIXSeconds
                             )
import Data.Time.Format ( formatTime, defaultTimeLocale
                        , rfc822DateFormat
                        )

getCurrentTimeInSeconds :: IO Int
getCurrentTimeInSeconds = (fromEnum . utcTimeToPOSIXSeconds) <$> getCurrentTime

toUTCTime :: Int -> UTCTime
toUTCTime = posixSecondsToUTCTime . toEnum

isoFormatCurrentUTCTime :: IO String
isoFormatCurrentUTCTime =
  (formatTime defaultTimeLocale rfc822DateFormat) <$> getCurrentTime
