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
import DMSS.Config

import Crypto.Gpgme

import Database.Persist.Sqlite
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Char8   as C

getCurrentTimeInSeconds :: IO Int
getCurrentTimeInSeconds = getCurrentTime >>= \t -> pure $ fromEnum $ utcTimeToPOSIXSeconds t

verifyCheckIn :: Fingerprint -> Entity CheckIn -> IO Bool
verifyCheckIn (Fingerprint f) e = do
  let rawCheckIn = checkInRaw_data $ entityVal e
  -- GPG context to verify raw data
  l <- gpgContext

  ret  <- verify' l (C.pack $ rawCheckIn)
  case ret of
    Left _ -> return False
    -- Make sure the checkin is Valid for the fingerprint
    Right l' -> return $ elem True $
            map (\(_, sigSummary, fpr) ->
                    ( (C.pack f) == fpr
                   && elem Valid sigSummary
                    )
                  ) (fst l')
