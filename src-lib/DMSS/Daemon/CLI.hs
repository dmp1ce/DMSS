-- |
-- Module      : DMSS.CLI
-- License     : Public Domain
--
-- Maintainer  : daveparrish@tutanota.com
-- Stability   : experimental
-- Portability : untested
--
-- Dead Man Switch System daemon CLI module
--
module DMSS.Daemon.CLI where

import DMSS.Daemon.Common (cliPort, peerPort)
import qualified Text.PrettyPrint.ANSI.Leijen as P ( text
                                                   , softline
                                                   , (<$>)
                                                   )
import Options.Applicative
  ( Parser, optional, strOption, long, metavar, help, execParser, info, helper
  , fullDesc, progDescDoc, header, flag, short, option, auto, value )
import Data.Monoid ( (<>) )
import Network.Socket (PortNumber)

data Cli = Cli
  { optHomedir :: Maybe String
  , optCliPort :: PortNumber
  , optPeerPort :: PortNumber
  , flagSilent :: FlagSilent
  }

data FlagSilent = SilentOff | SilentOn deriving Eq

daemonParser :: Parser Cli
daemonParser = Cli <$> optional (strOption
                        (  long "homedir"
                        <> metavar "DIRECTORY"
                        <> help "Change home directory"
                        ))
                   <*> option auto
                        (  long "cli-port"
                        <> value cliPort
                        <> metavar "PORT"
                        <> help "Custom CLI listening port"
                        )
                   <*> option auto
                        (  long "peer-port"
                        <> metavar "PORT"
                        <> value peerPort
                        <> help "Custom peer listening port"
                        )
                   <*> flag SilentOff SilentOn
                        (  long "silent"
                        <> short 's'
                        <> help "Silence output from daemon"
                        )

daemonMain :: (Cli -> IO ()) -> IO ()
daemonMain p = execParser opts >>= p
  where
    opts = info (helper <*> daemonParser)
      ( fullDesc
     <> progDescDoc ( Just (
                      P.text "Options for DMSS daemon."
                      P.<$> P.softline
                      P.<$> P.text "Use the '--help' flag to find more information about sub commands."
                      )
                    )
     <> header "DMSSd command line options" )
