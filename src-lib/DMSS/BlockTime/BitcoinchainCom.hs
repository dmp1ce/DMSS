-- |
-- Module      : DMSS.BlockTime.BitcoinchainCom
-- License     : Public Domain
--
-- Maintainer  : dino@ui3.info
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System: Acquire Bitcoin blockchain info via blockchain.info
--

{-# LANGUAGE OverloadedStrings #-}

module DMSS.BlockTime.BitcoinchainCom
   ( getBlockTime, getLatestBlock )
where

import DMSS.BlockTime.Types

import Control.Exception.Safe ( throw, tryAny )
import Control.Lens ( (^.) )
import Data.Aeson ( FromJSON, Value (Array, Object), (.:), decode, parseJSON )
import Data.Aeson.Types ( typeMismatch )
import Data.String.Conv ( toS )
import Data.Text ( Text )
import Data.Time ( UTCTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Data.Vector ( head )
import Network.Wreq ( get, responseBody )
import Prelude hiding ( head, null )


newtype BitcoinchainCom = BitcoinchainCom { unwrap :: BTCBlock }

instance FromJSON BitcoinchainCom where
  parseJSON (Array a) = parseJSON . head $ a

  parseJSON (Object o) = do
    block <- BTCBlock
      <$> (o .: "hash")
      <*> ((\epoch -> posixSecondsToUTCTime . realToFrac
          $ (epoch :: Int)) <$> (o .: "time"))
    return $ BitcoinchainCom block

  parseJSON invalid = typeMismatch "BTCBlock" invalid


getLatestBlock :: IO (Maybe BTCBlock)
getLatestBlock = either (const $ return Nothing) (return . Just) =<<
  ( tryAny $ do
    -- Retrieve the JSON document for the latest block (may fail)
    response <- get "https://api-r.bitcoinchain.com/v1/blocks/1"

    -- Decode the JSON in the response body and retrieve the block hash
    -- and timestamp (may fail)
    let body = response ^. responseBody
    maybe (throw BlockexplorerException) (return . unwrap) $ decode body
  )


getBlockTime :: Text -> IO (Maybe UTCTime)
getBlockTime hash = either (const $ return Nothing) (return . Just) =<<
  ( tryAny $ do
    -- Retrieve the JSON document for a specific block (may fail)
    response <- get $ concat ["https://blockchain.info/blocks/", (toS hash)]

    -- Decode the JSON in the response body and retrieve the block hash
    -- and timestamp (may fail)
    let body = response ^. responseBody
    maybe (throw BlockexplorerException) (return . blTime . unwrap) $ decode body
  )
