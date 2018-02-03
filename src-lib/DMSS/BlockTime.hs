-- |
-- Module      : DMSS.BlockTime
-- License     : Public Domain
--
-- Maintainer  : dino@ui3.info
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System time verification with Bitcoin blockchain
--

{-# LANGUAGE OverloadedStrings #-}

module DMSS.BlockTime
where

import Data.ByteString.Lazy ( ByteString )
import Control.Exception.Safe ( Exception, SomeException, Typeable,
  throw, tryAny )
import Control.Lens ( (^.) )
import Data.Aeson ( FromJSON, (.:), decode, parseJSON, withObject )
import Data.String.Conv ( toS )
import Data.Text hiding ( concat )
import Data.Time ( UTCTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import Network.Wreq ( get, responseBody )


newtype BlockexplorerException = BlockexplorerException ByteString
  deriving Typeable

instance Show BlockexplorerException where
  show (BlockexplorerException bs) = concat
    [ "Could not get block info from this block explorer. Response: "
    , (toS bs)
    ]

instance Exception BlockexplorerException


data BTCBlock = BTCBlock
  { blHash :: Text
  , blTime :: UTCTime
  }
  deriving Show

instance FromJSON BTCBlock where
  parseJSON = withObject "BTCBlock" $ \o -> BTCBlock
    <$> (o .: "hash")
    <*> ((\epoch -> posixSecondsToUTCTime . realToFrac
        $ (epoch :: Int)) <$> (o .: "time"))


getLatestBlock :: IO (Either SomeException BTCBlock)
getLatestBlock = tryAny $ do
  -- Retrieve the JSON document for the latest block (may fail)
  response <- get "https://blockchain.info/latestblock"

  -- Decode the JSON in the response body and retrieve the block hash
  -- and timestamp (may fail)
  let body = response ^. responseBody
  maybe (throw $ BlockexplorerException body) return $ decode body


getBlockTime :: Text -> IO (Either SomeException UTCTime)
getBlockTime hash = tryAny $ do
  -- Retrieve the JSON document for a specific block (may fail)
  response <- get $ concat ["https://blockchain.info/rawblock/", (toS hash)]

  -- Decode the JSON in the response body and retrieve the block hash
  -- and timestamp (may fail)
  let body = response ^. responseBody
  maybe (throw $ BlockexplorerException body) (return . blTime) $ decode body
