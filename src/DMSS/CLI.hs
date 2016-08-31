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

--import System.Environment (getArgs)
--import System.Daemon (runClient)
import Options.Applicative

--data Command = Id | Version

data Cli = Cli
  { optCommand :: Command }

data Command = Id | Version -- IdOptions

--data IdOptions = IdOptions
--  { test :: Bool }

--idOptions :: Parser IdOptions
--idOptions = pure IdOptions
--  <$> switch
--      ( long "test"
--     <> help "just for testing" )

sample :: Parser Cli
sample = Cli
  <$> subparser
      ( command "id" (info (pure Id)
        ( progDesc "Manage IDs" ))
     <> command "version" (info (pure Version)
        ( progDesc "Version info here"))
      )

greet :: Cli -> IO ()
greet (Cli Id) = putStrLn "Manage ID command" -- ++ " " ++ (show b)
greet (Cli Version) = putStrLn $ "Cli version: " ++ cliVersion
--greet _ = return ()

cliMain :: IO ()
cliMain = execParser opts >>= greet
  where
    opts = info (helper <*> sample)
      ( fullDesc
     <> progDesc "prog description"
     <> header "prog header" )
  
--  [n] <- getArgs
--  res <- runClient "localhost" 5000 n
--  print (res :: Maybe String)

cliVersion :: String
cliVersion = "0.1.0"
