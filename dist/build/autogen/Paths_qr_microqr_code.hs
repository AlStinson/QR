{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_qr_microqr_code (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Al Stinson\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Al Stinson\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3\\qr-microqr-code-0.1.0.0-7VfI9aXvUr670Dwg4puGUo"
dynlibdir  = "C:\\Users\\Al Stinson\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3"
datadir    = "C:\\Users\\Al Stinson\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.4.3\\qr-microqr-code-0.1.0.0"
libexecdir = "C:\\Users\\Al Stinson\\AppData\\Roaming\\cabal\\qr-microqr-code-0.1.0.0-7VfI9aXvUr670Dwg4puGUo\\x86_64-windows-ghc-8.4.3\\qr-microqr-code-0.1.0.0"
sysconfdir = "C:\\Users\\Al Stinson\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "qr_microqr_code_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "qr_microqr_code_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "qr_microqr_code_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "qr_microqr_code_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "qr_microqr_code_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "qr_microqr_code_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
