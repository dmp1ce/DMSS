-- |
-- Module      : DMSS.BlockTime.BlockTime
-- License     : Public Domain
--
-- Maintainer  : dino@ui3.info
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System time verification with Bitcoin blockchain
--

module DMSS.BlockTime.BlockTime
   ( getLatestBlock
   , getBlockTime
   )
where

import qualified DMSS.BlockTime.BitcoinchainCom as BitcoinchainCom
import qualified DMSS.BlockTime.BlockchainInfo as BlockchainInfo
import qualified DMSS.BlockTime.BtcCom as BtcCom
import DMSS.BlockTime.Types ( BlockexplorerException (..), BTCBlock )

import Control.Exception.Safe ( SomeException, throw, tryAny )
import Data.Monoid
import Data.Text ( Text )
import Data.Time ( UTCTime )


getLatestBlock :: IO (Either SomeException BTCBlock)
getLatestBlock = tryAny $ do
   mbBlocks <- sequence
      [ BlockchainInfo.getLatestBlock
      , BtcCom.getLatestBlock
      , BitcoinchainCom.getLatestBlock
      ]

   let mbFirst = getFirst . mconcat . map First $ mbBlocks
   maybe (throw BlockexplorerException) return mbFirst


getBlockTime :: Text -> IO (Either SomeException UTCTime)
getBlockTime hash = tryAny $ do
   mbTimes <- sequence $ map ($ hash)
      [ BlockchainInfo.getBlockTime
      , BtcCom.getBlockTime
      , BitcoinchainCom.getBlockTime
      ]

   let mbFirst = getFirst . mconcat . map First $ mbTimes
   maybe (throw BlockexplorerException) return mbFirst
