-- |
-- Module      : DMSS.Daemon
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System daemon module
--

{-# LANGUAGE OverloadedStrings #-}
module DMSS.Daemon where

--import Data.Default (def)
import System.Daemon
import Control.Pipe.C3 ( commandReceiver )
import Control.Concurrent ( forkIO, threadDelay )
--import Control.Monad ( forever )

import Crypto.Gpgme
import Turtle

checkerDaemon :: String -> IO String 
checkerDaemon str = do
  -- Start event loop if not started already
  _ <- forkIO $ forever $ do
    let ms = 1000
    threadDelay (ms * 1000)
    putStrLn $ "In event loop"
  putStrLn $ "Received request: " ++ str
  return str

daemonMain :: IO ()
daemonMain = do
  -- Start Daemon shortly after it starts
  _ <- forkIO $ do
    let ms = 1000
    threadDelay (ms * 1000)
    res <- runClient ("localhost"::String) 5000 ("Start Please"::String)
    print (res :: Maybe String)
  runInForeground 5000 (commandReceiver checkerDaemon)

-- GPG ID
type Identity = String

-- Unencrypted plain text message
type Message  = Plain

-- Encrypted list of messages
type LockBox  = [Encrypted]

createGpgIdentity :: IO ()
createGpgIdentity = do
  putStrLn "Trying to create GPG key here."
  -- Just use shell for now since h-gpgme doesn't have support for creating keys and I don't now how to use gpgme bindings yet.
  _ <- proc "gpg" ["--help"] empty
  return ()

daemonVersion :: String
daemonVersion = "0.1.0"
