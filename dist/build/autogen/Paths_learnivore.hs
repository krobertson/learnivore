module Paths_learnivore (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/lambda/.cabal/bin"
libdir     = "/Users/lambda/.cabal/lib/learnivore-0.1/ghc-6.12.3"
datadir    = "/Users/lambda/.cabal/share/learnivore-0.1"
libexecdir = "/Users/lambda/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "learnivore_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "learnivore_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "learnivore_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "learnivore_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
