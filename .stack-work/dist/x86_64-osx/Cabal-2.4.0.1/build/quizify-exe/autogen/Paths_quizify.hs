{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_quizify (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/local/git/quizify/.stack-work/install/x86_64-osx/lts-13.10/8.6.3/bin"
libdir     = "/Users/local/git/quizify/.stack-work/install/x86_64-osx/lts-13.10/8.6.3/lib/x86_64-osx-ghc-8.6.3/quizify-0.1-D3qlN5e8Qcy1uTFzekPdjA-quizify-exe"
dynlibdir  = "/Users/local/git/quizify/.stack-work/install/x86_64-osx/lts-13.10/8.6.3/lib/x86_64-osx-ghc-8.6.3"
datadir    = "/Users/local/git/quizify/.stack-work/install/x86_64-osx/lts-13.10/8.6.3/share/x86_64-osx-ghc-8.6.3/quizify-0.1"
libexecdir = "/Users/local/git/quizify/.stack-work/install/x86_64-osx/lts-13.10/8.6.3/libexec/x86_64-osx-ghc-8.6.3/quizify-0.1"
sysconfdir = "/Users/local/git/quizify/.stack-work/install/x86_64-osx/lts-13.10/8.6.3/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "quizify_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "quizify_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "quizify_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "quizify_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "quizify_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "quizify_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
