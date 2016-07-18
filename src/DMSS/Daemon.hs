module DMSS.Daemon where

--import Data.Default (def)
import System.Environment (getArgs)
import System.Daemon
import Control.Pipe.C3 ( commandReceiver )
import Control.Concurrent ( forkIO, threadDelay )
import Control.Monad ( forever )

checkerDaemon :: String -> IO String 
checkerDaemon s = do
  -- Start event loop if not started already
  _ <- forkIO $ forever $ do
    let ms = 1000
    threadDelay (ms * 1000)
    putStrLn $ "In event loop"
  putStrLn $ "Received request: " ++ s
  return s

daemonMain :: IO ()
daemonMain = do
  -- Start Daemon shortly after it starts
  _ <- forkIO $ do
    let ms = 1000
    threadDelay (ms * 1000)
    res <- runClient "localhost" 5000 "Start Please"
    print (res :: Maybe String)
  runInForeground 5000 (commandReceiver checkerDaemon)

cliMain :: IO ()
cliMain = do
  [n] <- getArgs
  res <- runClient "localhost" 5000 n
  print (res :: Maybe String)

-- Current version for daemon and cli
daemonVersion :: String
daemonVersion = "v0.2.0"
