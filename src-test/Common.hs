module Common where
  
import System.IO.Temp
import System.Environment

import DMSS.Config

withTemporaryTestDirectory :: FilePath -> (FilePath -> IO a) -> IO a
withTemporaryTestDirectory t f = withSystemTempDirectory t ( \s -> do
    -- Change HOME environment variable to temporary directory
    setEnv "HOME" s
    -- Create local home directory for DMSS
    createLocalDirectory
    -- Run test
    f s
  )

withTemporaryTestStorage :: FilePath -> (FilePath -> IO a) -> IO a
withTemporaryTestStorage t f = withTemporaryTestDirectory t ( \s -> do
    -- Run test
    f s
  )
