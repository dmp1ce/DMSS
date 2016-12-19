#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-
Run DMSS tests specified
-}

{-# LANGUAGE OverloadedStrings #-}

import Turtle

desc :: Description
desc = Description "Run tests for DMSS. To run all tests just run this script without any arguments."

data UserInput = UserInput Bool (Maybe Text)

parser :: Parser UserInput
parser = UserInput <$> switch "no-prompt" 'n'
                       "Only run tests that don't have a prompt (ignored if argument is specified)"
                   <*> optional (argText "str" "Run test with this substring in test title")

main :: IO ExitCode
main = do
  userInput <- options desc parser
  case userInput of
    UserInput False Nothing -> runTestsWithPrompt Nothing
    UserInput True  Nothing -> runTestsWithoutPrompt Nothing
    UserInput False str     -> runTestsWithPrompt str
    UserInput True  str     -> runTestsWithoutPrompt str
  where
    runTestsWithPrompt Nothing       = shell "stack test" empty
    runTestsWithPrompt (Just s')     = shell (format ("stack test --test-arguments \"-p "%s%"\"") s') empty
    runTestsWithoutPrompt Nothing    = shell "stack test --test-arguments \"-p !**/*_prompt*\"" empty
    runTestsWithoutPrompt (Just s')  = do
      echo (format ("Ignoring '-n' argument ("%s%") because string argument was specified") s')
      runTestsWithPrompt (Just s')
