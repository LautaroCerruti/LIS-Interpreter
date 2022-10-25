{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_TP1 (
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

bindir     = "E:\\Facu\\3\176\\2\176 Cuat\\ALP\\TP1\\.stack-work\\install\\adf9955a\\bin"
libdir     = "E:\\Facu\\3\176\\2\176 Cuat\\ALP\\TP1\\.stack-work\\install\\adf9955a\\lib\\x86_64-windows-ghc-8.8.3\\TP1-0.1.0.0-Jb3fQuRG8SO5hPu3thChUC"
dynlibdir  = "E:\\Facu\\3\176\\2\176 Cuat\\ALP\\TP1\\.stack-work\\install\\adf9955a\\lib\\x86_64-windows-ghc-8.8.3"
datadir    = "E:\\Facu\\3\176\\2\176 Cuat\\ALP\\TP1\\.stack-work\\install\\adf9955a\\share\\x86_64-windows-ghc-8.8.3\\TP1-0.1.0.0"
libexecdir = "E:\\Facu\\3\176\\2\176 Cuat\\ALP\\TP1\\.stack-work\\install\\adf9955a\\libexec\\x86_64-windows-ghc-8.8.3\\TP1-0.1.0.0"
sysconfdir = "E:\\Facu\\3\176\\2\176 Cuat\\ALP\\TP1\\.stack-work\\install\\adf9955a\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TP1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TP1_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "TP1_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "TP1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TP1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TP1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
