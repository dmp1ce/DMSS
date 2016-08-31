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
  

data Command = Id IdOptions

data IdOptions = IdOptions
--  { test :: Bool }

idOptions :: Parser IdOptions
idOptions = pure IdOptions
--  <$> switch
--      ( long "test"
--     <> help "just for testing" )

sample :: Parser Command
sample = Id
  <$> subparser
      ( command "id" (info idOptions
        ( progDesc "Manage IDs" )))

greet :: Command -> IO ()
greet (Id IdOptions) = putStrLn "Manage ID command go" -- ++ " " ++ (show b)
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
