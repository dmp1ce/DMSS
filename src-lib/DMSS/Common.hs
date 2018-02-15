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
                   , toSeconds
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
getCurrentTimeInSeconds = toSeconds <$> getCurrentTime

toSeconds :: UTCTime -> Int
toSeconds = round . utcTimeToPOSIXSeconds

toUTCTime :: Int -> UTCTime
toUTCTime = posixSecondsToUTCTime . fromInteger . toInteger

isoFormatCurrentUTCTime :: IO String
isoFormatCurrentUTCTime =
  (formatTime defaultTimeLocale rfc822DateFormat) <$> getCurrentTime
