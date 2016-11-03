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
import DMSS.Config

import Crypto.Gpgme
import qualified  Crypto.Gpgme.Key.Gen   as G

import System.Daemon (runClient)
import Data.Maybe (fromJust)
import Text.Email.Validate
import qualified  Text.PrettyPrint       as PP
import Options.Applicative
import Data.Default (def)
--import Data.List
import qualified Data.ByteString.Char8   as C
--import qualified Data.ByteString       as BS
import qualified Text.PrettyPrint.ANSI.Leijen as P ( text
                                                   , line
                                                   , (<$>)
                                                   )

data Cli = Cli
  { optCommand :: Command }

cliParser :: Parser Cli
cliParser = Cli <$> idParser

idParser :: Parser Command
idParser = hsubparser
  ( command "id" (info (Id <$> idCommandParser)
    ( progDesc "Manage IDs" ))
 <> command "status" (info (pure Status)
    ( progDesc "Some status about the system"))
 <> command "version" (info (pure Version)
    ( progDesc "Version info here"))
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
process (Cli (Id (IdCreate Nothing e))) = do
  putStrLn "Please enter the name for the ID:"
  n <- getLine
  process $ Cli $ Id $ IdCreate (Just n) e
process (Cli (Id (IdCreate (Just n) e))) = do
  -- Create GPG id
  l <- gpgContext
  let params = (def :: G.GenKeyParams)
        { G.keyType = Just Dsa
        , G.nameReal = C.pack n
        , G.nameEmail = (emailAddress . C.pack) $ maybe "" id e
        }
  createLocalDirectory
  ret <- withCtx l "C" OpenPGP $ \ctx ->
    G.genKey ctx params
  putStrLn $ show ret

process (Cli (Id IdList)) = do
  putStrLn "ID List command"
  l <- gpgContext
  res <- withCtx l "C" OpenPGP $ \ctx ->
    listKeys ctx WithSecret
  ids <- mapM (\k -> do
              i <- keyUserIds' k
              s <- keySubKeys' k
              return (i,s)
            ) res
  putStrLn $ PP.render (draw $ map (\i -> (fst i, snd i)) ids)
  where
    draw :: [([KeyUserId], [SubKey])] -> PP.Doc
    draw xs =
      row "NAME" "EMAIL" "FINGERPRINT" PP.$+$ PP.vcat (map dataRow xs)
    row n e f =
      let nameColWidth = 15
          emailColWidth = 30
       in PP.text (ellipsis nameColWidth n)
          PP.$$ PP.nest nameColWidth (PP.text (ellipsis emailColWidth e))
          PP.$$ PP.nest (nameColWidth+emailColWidth) (PP.text f)
    ellipsis n s
      | ((length s) > (n-4)) = (take (n-4) s) ++ "..."
      | otherwise            = s
    dataRow :: ([KeyUserId], [SubKey]) -> PP.Doc
    dataRow (names, subkeys) =
      row (cc (userName . keyuserId) names)
        (cc (userEmail . keyuserId) names)
        (cc (C.unpack . subkeyFpr) subkeys)
      where
        cc f l = unwords (foldr (\n a -> (f n):a) [] l)

process (Cli (Id (IdRemove fpr))) = do
  putStrLn "ID Remove command"
  l <- gpgContext
  ret <- withCtx l "C" OpenPGP $ \ctx -> do
    key <- getKey ctx (C.pack fpr) WithSecret
    removeKey ctx (fromJust key) WithSecret
  putStrLn $ show ret

process (Cli Status) = do
  res <- runCommand Status
  print (res :: Maybe String)
process (Cli Version) = do
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
     <> progDescDoc ( Just ( P.text "CLI for managing the DMSS."
                      P.<$> P.line
                      P.<$> P.text "Commands: "
                      P.<$> P.text "id create - Create an ID"
                      P.<$> P.text "id list   - List an IDs"
                      P.<$> P.text "id remove - Remove an ID")
                    )
     <> header "DMSS Command Line Interface" )

cliVersion :: String
cliVersion = "0.1.0"
