-- |
-- Module      : DMSS.Daemon
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System daemon memory
--

module DMSS.Daemon.Memory where

import Control.Concurrent.STM.TVar (TVar)

-- Manages concurrency between application threads
type DaemonTVar = TVar (String,Int)
