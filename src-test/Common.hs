module Common where
  
import System.Unix.Directory
--import System.Directory
import System.Environment

import DMSS.Config

withTemporaryTestDirectory :: FilePath -> (FilePath -> IO a) -> IO a
withTemporaryTestDirectory t f = withTemporaryDirectory t ( \s -> do
    -- Change HOME environment variable to temporary directory
    setEnv "HOME" s
    -- Create local home directory for DMSS
    createLocalDirectory
    f s
  )
