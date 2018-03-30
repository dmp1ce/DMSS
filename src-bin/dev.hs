{-
Run DMSS tests specified
-}

import Turtle
import Data.List (intersperse)
import Prelude hiding (concat)
import Data.Text (concat)
import Data.Foldable (traverse_)
import System.Process (spawnCommand, waitForProcess)
import Data.Text.Conversions (convertText)
import Paths_DMSS ( version )
import Data.Version ( showVersion )

desc :: Description
desc = Description "Run tests for DMSS. To run all tests just run this script without any arguments."

data UserInput = Test Bool (Maybe Text) | Version deriving Show

parser :: Parser UserInput
parser = Test <$> subcommand "test" "Run tests" (switch "watch" 'w' "Watch for file changes.")
              <*> optional (optText "pattern" 'p' "Run test with this pattern in test title")
     <|> Version <$ subcommand "version" "Show version" empty

stackCommand :: Text
stackCommand = "stack test --fast --haddock-deps"

targets :: [Text]
targets = [ ":DMSS-test"
          , ":dev"
          , ":dmss"
          , ":dmssd"
          ]

main :: IO ExitCode
main = do
  userInput <- options desc parser
  case userInput of
    Test False Nothing -> runTests Nothing
    Test True  Nothing -> watchTests Nothing
    Test False str     -> runTests str
    Test True  str     -> watchTests str
    Version            -> do
      traverse_ echo (textToLine $ convertText $ showVersion version)
      exit ExitSuccess
  where
    watchTests Nothing  = shell' (format (s%" --file-watch "%s) stackCommand targetsString)
    watchTests (Just p) = shell' (format
                                      (s%" --file-watch "%s%" "%s)
                                      stackCommand targetsString (testArgs p))
    runTests Nothing   = shell' (format (s%" "%s) stackCommand targetsString)
    runTests (Just p)  = shell' (format (s%" "%s%" "%s) stackCommand targetsString (testArgs p))
    targetsString = concat $ intersperse " " targets
    testArgs = format ("--test-arguments '-p \""%s%"\"'")
    -- Shell which allows for user input
    -- https://github.com/Gabriel439/Haskell-Turtle-Library/issues/275
    shell' :: Text -> IO ExitCode
    shell' t = do
      traverse_ echo $ textToLine t
      spawnCommand (convertText t) >>= waitForProcess
