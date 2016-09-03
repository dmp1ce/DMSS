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
import qualified Text.PrettyPrint.ANSI.Leijen as P ( text
                                                   , line
                                                   , (<$>)
                                                   )
--import DMSS.Config

data Cli = Cli
  { optCommand :: Command }

cliParser :: Parser Cli
cliParser = Cli <$> idParser

idParser :: Parser Command
idParser = hsubparser
  ( command "id" (info (Id <$> idCommandParser)
    ( progDesc "Manage IDs" ))
 <> command "version" (info (pure Version)
    ( progDesc "Version info here"))
  )

idCommandParser :: Parser IdCommand
idCommandParser = hsubparser
  ( command "create" (info (pure IdCreate)
    ( progDesc "Create IDs" ))
 <> command "list" (info (pure IdList)
    ( progDesc "List IDs" ))
  )


--idOptions :: Parser IdOptions
--idOptions = pure IdOptions
--  <$> switch
--      ( long "test"
--     <> help "just for testing" )


process :: Cli -> IO ()
process (Cli (Id IdCreate)) = do
  putStrLn "ID Create command"
  res <- runClient "localhost" 5000 (Id IdCreate)
  print (res :: Maybe String)
process (Cli (Id IdList)) = do
  putStrLn "ID List command"
  res <- runClient "localhost" 5000 (Id IdList)
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
     <> progDescDoc ( Just ( P.text "CLI for managing the DMSS."
                      P.<$> P.line
                      P.<$> P.text "Commands: "
                      P.<$> P.text "id create - Will create an ID"
                      P.<$> P.text "id list - Will list an IDs")
                    )
     <> header "DMSS Command Line Interface" )

cliVersion :: String
cliVersion = "0.1.0"
