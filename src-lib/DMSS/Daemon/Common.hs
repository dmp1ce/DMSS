-- |
-- Module      : DMSS.Common
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System Daemon command module
--

module DMSS.Daemon.Common where

import Network.Socket (PortNumber)

cliPort :: PortNumber
cliPort = 5006

peerPort :: PortNumber
peerPort = 5007
