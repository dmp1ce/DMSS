-- |
-- Module      : DMSS.CLI
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System CLI module
--

module DMSS.CLI where

import DMSS.Command

import System.Daemon (runClient)
import Options.Applicative

--data Command = Id | Version

data Cli = Cli
  { optCommand :: Command }

--data IdOptions = IdOptions
--  { test :: Bool }

--idOptions :: Parser IdOptions
--idOptions = pure IdOptions
--  <$> switch
--      ( long "test"
--     <> help "just for testing" )

cliParser :: Parser Cli
cliParser = Cli
  <$> subparser
      ( command "id" (info (pure Id)
        ( progDesc "Manage IDs" ))
     <> command "version" (info (pure Version)
        ( progDesc "Version info here"))
      )

process :: Cli -> IO ()
process (Cli Id) = do
  putStrLn "Manage ID command" -- ++ " " ++ (show b)
  res <- runClient "localhost" 5000 Id
  print (res :: Maybe String)
process (Cli Version) = do
  putStrLn $ "CLI version: " ++ cliVersion
  res <- runClient "localhost" 5000 Version
  print (res :: Maybe String)

cliMain :: IO ()
cliMain = execParser opts >>= process
  where
    opts = info (helper <*> cliParser)
      ( fullDesc
     <> progDesc "prog description"
     <> header "prog header" )
  

cliVersion :: String
cliVersion = "0.1.0"
