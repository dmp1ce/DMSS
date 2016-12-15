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
import DMSS.Storage ( storeCheckIn
                    , storeUserKey
                    , removeUserKey
                    , Fingerprint (..)
                    , CheckInProof (..)
                    )

import Crypto.Gpgme
import qualified  Crypto.Gpgme.Key.Gen   as G

import System.Daemon (runClient)
import Data.Maybe (fromJust)
import Text.Email.Validate
import qualified  Text.PrettyPrint       as PP
import Options.Applicative
import Data.Default (def)
import qualified Data.ByteString.Char8   as C
import qualified Text.PrettyPrint.ANSI.Leijen as P ( text
                                                   , softline
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
  ret <- withCtx l "C" OpenPGP $ \ctx -> do
    eitherFpr <- G.genKey ctx params
    either (\_ -> return ("genKey failed with return: " ++ show eitherFpr))
      (\fpr -> do
          s <- storeUserKey $ Fingerprint $ C.unpack fpr
          return $ show s)
      eitherFpr
  putStrLn $ show ret

process (Cli (Id IdList)) = do
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
  -- Remove from Storage
  removeUserKey (Fingerprint fpr)

  l <- gpgContext
  ret <- withCtx l "C" OpenPGP $ \ctx -> do
    key <- getKey ctx (C.pack fpr) WithSecret

    -- Remove from GPG context
    removeKey ctx (fromJust key) WithSecret
  case ret of
    Nothing  -> return ()
    (Just e) -> putStrLn $ show e

process (Cli (CheckIn (CheckInCreate fpr))) = do
  putStrLn $ "CheckIn for " ++ fpr
  l <- gpgContext
  eitherCT <- withCtx l "C" OpenPGP $ \ctx -> do
    maybeKey <- getKey ctx (C.pack fpr) WithSecret
    let key = maybe (error ("Invalid key id " ++ fpr)) id maybeKey
    clearSign ctx [key] (C.pack "Logged in at this date 2016-12-07")
  let ct = either (\e -> error $ show e) id eitherCT
  _ <- storeCheckIn (Fingerprint fpr) (CheckInProof $ C.unpack ct)
  return ()

process (Cli (CheckIn _)) = do
  putStrLn "CheckIn command here"

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
