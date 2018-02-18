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

import qualified Text.PrettyPrint.ANSI.Leijen as P ( text
                                                   , softline
                                                   , (<$>)
                                                   )
import Options.Applicative
  ( Parser, optional, strOption, long, metavar, help, execParser, info, helper
  , fullDesc, progDescDoc, header )
import Data.Default (def, Default)
import Data.Monoid ( (<>) )

newtype Options = Options { daemonSilent :: Bool }

instance Default Options where
  def = Options False

defaultOptions :: Options
defaultOptions = def :: Options

data Cli = Cli
  { optHomedir :: Maybe String }

daemonParser :: Parser Cli
daemonParser = Cli <$> optional (strOption ( long "homedir"
                         <> metavar "DIRECTORY"
                         <> help "Change home directory"
                          )
                       )
                   -- <*> idParser

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
