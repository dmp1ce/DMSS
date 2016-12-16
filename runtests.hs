#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle

{-
Run DMSS tests specified
-}

{-# LANGUAGE OverloadedStrings #-}

import Turtle

desc :: Description
desc = Description "Run tests for DMSS. To run all tests just run this script without any arguments."

data UserInput = UserInput (Maybe Text)

parser :: Parser UserInput
parser = UserInput <$> optional (argText "str" "Run test with this substring in test title")

main :: IO ExitCode
main = do
  userInput <- options desc parser
  case userInput of
    UserInput Nothing    -> shell "stack test" empty
    UserInput (Just str) -> shell (format ("stack test --test-arguments \"-p "%s%"\"") str) empty
