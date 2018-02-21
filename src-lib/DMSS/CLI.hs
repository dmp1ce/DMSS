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

import           DMSS.CLI.Command
import           DMSS.CLI.Internal
import           DMSS.Storage ( Name (..))
import           DMSS.Daemon.Common ( cliPort )

import Data.String ( fromString )
import qualified DMSS.Daemon.Command as DCLI
import           Paths_DMSS ( version )

import           Data.Monoid ((<>))
import           Data.Version ( showVersion )
import           Options.Applicative
import           System.Daemon ( runClient )
import           System.Environment (setEnv)
import           Data.Foldable (traverse_)
import           Network.Socket (PortNumber)
import qualified Text.PrettyPrint.ANSI.Leijen as P ( text
                                                   , softline
                                                   , (<$>)
                                                   )

data Cli = Cli
  { optHomedir :: Maybe String
  , optPort :: PortNumber
  , flagSilent :: FlagSilent
  , optCommand :: Command }

data FlagSilent = SilentOff | SilentOn deriving Eq

cliParser :: Parser Cli
cliParser = Cli <$> optional (strOption ( long "homedir"
                                       <> metavar "DIRECTORY"
                                       <> help "Change home directory"
                                        )
                             )
                <*> option auto
                        (  long "port"
                        <> value cliPort
                        <> metavar "PORT"
                        <> help "Daemon CLI port to connect to"
                        )
                <*> flag SilentOff SilentOn
                     (  long "silent"
                     <> short 's'
                     <> help "Silence output from CLI where possible. \
                             \Still allows prompting the user."
                     )
                <*> commandParser

commandParser :: Parser Command
commandParser = hsubparser
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
  ( command "create" (info ( CheckInCreate <$> argument str (metavar "NAME"))
    (progDesc "Create CheckIn proof" ))
 <> command "list" (info (pure CheckInList) (progDesc "List CheckIns" ))
  )

idCommandParser :: Parser IdCommand
idCommandParser = hsubparser
  ( command "create" (info ( IdCreate
                           <$> nameOption
                           <*> passwordOption
                           )
    ( progDesc "Create IDs" ))
 <> command "remove" (info ( IdRemove
                           <$> argument str (metavar "NAME")
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
    passwordOption = optional $ strOption
      ( long "password"
     <> short 'p'
     <> help "Password to encrypt user secret on this device" )

process :: Cli -> IO ()
process (Cli (Just homeStr) p s c) = do
  -- Set home directory
  setEnv "HOME" homeStr
  process (Cli Nothing p s c)

process (Cli Nothing p s (Id (IdCreate Nothing e))) = do
  putStrLn "Please enter the name for the ID:"
  n <- getLine
  process $ Cli Nothing p s $ Id $ IdCreate (Just n) e
process (Cli Nothing pn s (Id (IdCreate n Nothing))) = do
  putStrLn "Please enter password to encrypt keys with."
  p <- getLine
  putStrLn "Please ensure that you remember the password"
  putStrLn "by entering it again. Your identity will be"
  putStrLn "unusable if you forget your password!"
  p' <- getLine
  if p /= p'
  then do putStrLn "Passwords did not match."
          process $ Cli Nothing pn s $ Id $ IdCreate n Nothing
  else process $ Cli Nothing pn s $ Id $ IdCreate n (Just p)

process (Cli Nothing _ s (Id (IdCreate (Just n) (Just p)))) =
  processIdCreate n p >> msgLn s (n ++ " created")
process (Cli Nothing _ s (Id IdList)) = processIdList >>= msgLn s
process (Cli Nothing _ s (Id (IdRemove fpr))) =
  processIdRemove fpr >>= traverse_ (msgLn s)

process (Cli Nothing _ _ (CheckIn (CheckInCreate n))) = do
  putStrLn $ "Please enter password for " ++ n ++ ": "
  p <- getLine
  processCheckInCreate (Name n) (fromString p)

process (Cli Nothing _ _ (CheckIn CheckInList)) =
  putStrLn "TODO: CheckIn List command here"

process (Cli Nothing pn s Status) = do
  res <- runCommand pn DCLI.Status
  traverse_ (msgLn s) res
process (Cli Nothing pn s Version) = do
  msgLn s $ "CLI version: " ++ showVersion version
  res <- runCommand pn DCLI.Version
  traverse_ (msgLn s) res

runCommand :: PortNumber -> DCLI.Command -> IO (Maybe String)
runCommand = (runClient "localhost") . fromIntegral

cliMain :: IO ()
cliMain = execParser opts >>= process
  where
    opts = info (helper <*> cliParser)
      ( fullDesc
     <> progDescDoc ( Just (
                      P.text "CLI for managing the DMSS."
                      P.<$> P.softline
                      P.<$> P.text "Use the '--help' flag to find more information about sub commands."
                      P.<$> P.text "For example 'dmss id --help' or 'dmss id create -h'."
                      )
                    )
     <> header "DMSS Command Line Interface" )

msgLn :: FlagSilent -> String -> IO ()
msgLn s msg = if s == SilentOff then putStrLn msg else return ()
