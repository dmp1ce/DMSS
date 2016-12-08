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

-- | Default data directory for DMSS
localDirectory :: IO String
localDirectory = getEnv "HOME" >>= \h -> pure $ h ++ "/.local/share/dmss"

-- | GPG context directory location
gpgContext :: IO String
gpgContext = localDirectory >>= \l -> pure $ l ++ "/gpg"

-- | Creates the data directory if it doesn't exist already
createLocalDirectory :: IO ()
createLocalDirectory = gpgContext >>= createDirectoryIfMissing True
