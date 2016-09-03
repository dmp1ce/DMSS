-- |
-- Module      : DMSS.Command
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System config module
--
module DMSS.Config where

import System.Environment ( getEnv )
import System.Directory   ( createDirectoryIfMissing )

localDirectory :: IO String
localDirectory = getEnv "HOME" >>= \h -> pure $ h ++ "/.local/share/dmss"

gpgContext :: IO String
gpgContext = localDirectory >>= \l -> pure $ l ++ "/gpg"

createLocalDirectory :: IO ()
createLocalDirectory = gpgContext >>= createDirectoryIfMissing True
