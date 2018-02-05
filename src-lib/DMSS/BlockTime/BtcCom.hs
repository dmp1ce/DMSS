-- |
-- Module      : DMSS.BlockTime.BtcCom
-- License     : Public Domain
--
-- Maintainer  : dino@ui3.info
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System: Acquire Bitcoin blockchain info via btc.com
--

{-# LANGUAGE OverloadedStrings #-}

module DMSS.BlockTime.BtcCom
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


newtype BtcCom = BtcCom { unwrap :: BTCBlock }

instance FromJSON BtcCom where
  parseJSON = withObject "BTCBlock" $ \o -> do
    datao <- o .: "data"
    block <- BTCBlock
      <$> (datao .: "hash")
      <*> ((\epoch -> posixSecondsToUTCTime . realToFrac
          $ (epoch :: Int)) <$> (datao .: "timestamp"))
    return $ BtcCom block


getLatestBlock :: IO (Maybe BTCBlock)
getLatestBlock = either (const $ return Nothing) (return . Just) =<<
  ( tryAny $ do
    -- Retrieve the JSON document for the latest block (may fail)
    response <- get "https://chain.api.btc.com/v3/block/latest"

    -- Decode the JSON in the response body and retrieve the block hash
    -- and timestamp (may fail)
    let body = response ^. responseBody
    maybe (throw BlockexplorerException) (return . unwrap) $ decode body
  )


getBlockTime :: Text -> IO (Maybe UTCTime)
getBlockTime hash = either (const $ return Nothing) (return . Just) =<<
  ( tryAny $ do
    -- Retrieve the JSON document for a specific block (may fail)
    response <- get $ concat ["https://chain.api.btc.com/v3/block/", (toS hash)]

    -- Decode the JSON in the response body and retrieve the block hash
    -- and timestamp (may fail)
    let body = response ^. responseBody
    maybe (throw BlockexplorerException) (return . blTime . unwrap) $ decode body
  )
