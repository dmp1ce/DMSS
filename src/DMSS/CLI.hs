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
import DMSS.CLI.Internal

import System.Daemon (runClient)
import System.Environment (setEnv)
import Options.Applicative
import Data.Monoid ((<>))
import qualified Text.PrettyPrint.ANSI.Leijen as P ( text
                                                   , softline
                                                   , (<$>)
                                                   )

data Cli = Cli
  { optHomedir :: Maybe String
  , optCommand :: Command }

cliParser :: Parser Cli
cliParser = Cli <$> (optional $ strOption ( long "homedir"
                                        <> metavar "DIRECTORY"
                                        <> help "Change home directory"
                                         ))
                <*> idParser

idParser :: Parser Command
idParser = hsubparser
  ( command "id" (info (Id <$> idCommandParser)
    ( progDesc "Manage IDs" ))
 <> command "checkin" (info (CheckIn <$> checkInCommandParser)
    ( progDesc "Manage CheckIns"))
 <> command "status" (info (pure Status)
    ( progDesc "Some status about the system"))
 <> command "version" (info (pure Version)
    ( progDesc "Version info here"))
  )

checkInCommandParser :: Parser CheckInCommand
checkInCommandParser = hsubparser
  ( command "create" (info ( CheckInCreate <$> argument str (metavar "FINGERPRINT"))
    (progDesc "Create CheckIn proof" ))
 <> command "list" (info (pure CheckInList) (progDesc "List CheckIns" ))
  )

idCommandParser :: Parser IdCommand
idCommandParser = hsubparser
  ( command "create" (info ( IdCreate
                           <$> nameOption
                           <*> emailOption
                           )
    ( progDesc "Create IDs" ))
 <> command "remove" (info ( IdRemove
                           <$> argument str (metavar "FINGERPRINT")
                           )
    ( progDesc "Remove ID" ))
 <> command "list" (info (pure IdList)
    ( progDesc "List IDs" ))
  )
  where
    nameOption = optional $ strOption
      ( long "name"
     <> short 'n'
     <> help "Name of ID" )
    emailOption = optional $ strOption
      ( long "email"
     <> short 'e'
     <> value ""
     <> help "Email of ID" )

process :: Cli -> IO ()
process (Cli (Just homeStr) c) = do
  -- Set home directory
  setEnv "HOME" homeStr
  process (Cli Nothing c)

process (Cli Nothing (Id (IdCreate Nothing e))) = do
  putStrLn "Please enter the name for the ID:"
  n <- getLine
  process $ Cli Nothing $ Id $ IdCreate (Just n) e
process (Cli Nothing (Id (IdCreate (Just n) e))) = processIdCreate n e >>= putStrLn
process (Cli Nothing (Id IdList)) = processIdList >>= putStrLn
process (Cli Nothing (Id (IdRemove fpr))) = do
  m <- processIdRemove fpr
  case m of
    Nothing -> return ()
    Just s -> putStrLn s

process (Cli Nothing (CheckIn (CheckInCreate fpr))) =
  processCheckInCreate fpr

process (Cli Nothing (CheckIn CheckInList)) = do
  putStrLn "CheckIn List command here"

process (Cli Nothing Status) = do
  res <- runCommand Status
  print (res :: Maybe String)
process (Cli Nothing Version) = do
  putStrLn $ "CLI version: " ++ cliVersion
  res <- runCommand Version
  print (res :: Maybe String)

runCommand :: Command -> IO (Maybe String)
runCommand c = runClient "localhost" 5000 c

cliMain :: IO ()
cliMain = execParser opts >>= process
  where
    opts = info (helper <*> cliParser)
      ( fullDesc
     <> progDescDoc ( Just (
                      P.text "CLI for managing the DMSS."
                      P.<$> P.softline
                      P.<$> P.text "Use the '--help' flag to find more information about sub commands."
                      P.<$> P.text "For example 'dmss-cli id --help' or 'dmss-cli id create -h'."
                      )
                    )
     <> header "DMSS Command Line Interface" )

cliVersion :: String
cliVersion = "0.1.0"
