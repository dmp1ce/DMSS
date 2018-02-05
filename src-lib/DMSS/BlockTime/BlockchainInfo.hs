-- |
-- Module      : DMSS.BlockTime.BlockchainInfo
-- License     : Public Domain
--
-- Maintainer  : dino@ui3.info
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System: Acquire Bitcoin blockchain info via blockchain.info
--

{-# LANGUAGE OverloadedStrings #-}

module DMSS.BlockTime.BlockchainInfo
   ( getBlockTime, getLatestBlock )
where

import DMSS.BlockTime.Types

import Control.Exception.Safe ( throw, tryAny )
import Control.Lens ( (^.) )
import Data.Aeson ( FromJSON, (.:), decode, parseJSON, withObject )
import Data.String.Conv ( toS )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Network.Wreq ( get, responseBody )


newtype BlockchainInfo = BlockchainInfo { unwrap :: BTCBlock }

instance FromJSON BlockchainInfo where
  parseJSON = withObject "BTCBlock" $ \o -> do
    block <- BTCBlock
      <$> (o .: "hash")
      <*> ((\epoch -> posixSecondsToUTCTime . realToFrac
          $ (epoch :: Int)) <$> (o .: "time"))
    return $ BlockchainInfo block


getLatestBlock :: IO (Maybe BTCBlock)
getLatestBlock = either (const $ return Nothing) (return . Just) =<<
  ( tryAny $ do
    -- Retrieve the JSON document for the latest block (may fail)
    response <- get "https://blockchain.info/latestblock"

    -- Decode the JSON in the response body and retrieve the block hash
    -- and timestamp (may fail)
    let body = response ^. responseBody
    maybe (throw BlockexplorerException) (return . unwrap) $ decode body
  )


getBlockTime :: Text -> IO (Maybe UTCTime)
getBlockTime hash = either (const $ return Nothing) (return . Just) =<<
  ( tryAny $ do
    -- Retrieve the JSON document for a specific block (may fail)
    response <- get $ concat ["https://blockchain.info/rawblock/", (toS hash)]

    -- Decode the JSON in the response body and retrieve the block hash
    -- and timestamp (may fail)
    let body = response ^. responseBody
    maybe (throw BlockexplorerException) (return . blTime . unwrap) $ decode body
  )
