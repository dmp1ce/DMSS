module Common where
  
import System.IO.Temp
import System.Environment
import Data.List
import System.FilePath    ((</>))
import System.Directory   (  createDirectory
                          , listDirectory
                          , copyFile
                          )

import DMSS.Config

withTemporaryAliceHome :: FilePath -> (FilePath -> IO a) -> IO a
withTemporaryAliceHome t func = withTemporaryTestDirectory t ( \tmpDir -> do
    -- Copy sqlite database
    copyFile "src-test/fixtures/alice_home/.local/share/dmss/dmss.sqlite"
             (tmpDir </> ".local/share/dmss/dmss.sqlite")
    -- Copy alice's key into temporary directory so we can safely remove it
    alice_files <- listDirectory "src-test/fixtures/alice_home/.local/share/dmss/gpg"
    mapM_ (\f -> copyFile ("src-test/fixtures/alice_home/.local/share/dmss/gpg" </> f)
                          (tmpDir </> ".local/share/dmss/gpg" </> f))
      $ filter (\f -> (not $ isPrefixOf "S.gpg-agent" f)
                 && f /= "private-keys-v1.d"
                 && f /= "openpgp-revocs.d"
                 && f /= ".gpg-v21-migrated"
                 && f /= "random_seed"
             ) alice_files
    -- Copy subdirectories the manual way
    createDirectory (tmpDir </> ".local/share/dmss/gpg/private-keys-v1.d")
    pKeyFiles   <- listDirectory "src-test/fixtures/alice_home/.local/share/dmss/gpg/private-keys-v1.d"
    mapM_ (\f -> copyFile ("src-test/fixtures/alice_home/.local/share/dmss/gpg/private-keys-v1.d" </> f)
                          (tmpDir </> ".local/share/dmss/gpg/private-keys-v1.d" </> f)
                   ) pKeyFiles

    createDirectory (tmpDir </> ".local/share/dmss/gpg/openpgp-revocs.d")
    revocsFiles <- listDirectory "src-test/fixtures/alice_home/.local/share/dmss/gpg/openpgp-revocs.d"
    mapM_ (\f -> copyFile ("src-test/fixtures/alice_home/.local/share/dmss/gpg/openpgp-revocs.d" </> f)
                          (tmpDir </> ".local/share/dmss/gpg/openpgp-revocs.d" </> f)
                   ) revocsFiles

    func tmpDir
  )

withTemporaryTestDirectory :: FilePath -> (FilePath -> IO a) -> IO a
withTemporaryTestDirectory t f = withSystemTempDirectory t ( \s -> do
    -- Change HOME environment variable to temporary directory
    setEnv "HOME" s
    -- Create local home directory for DMSS
    createLocalDirectory
    -- Run test
    f s
  )
