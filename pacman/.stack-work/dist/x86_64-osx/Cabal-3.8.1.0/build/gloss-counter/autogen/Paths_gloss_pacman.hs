{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_gloss_pacman (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/espennoreng/Desktop/pacman/pacman/.stack-work/install/x86_64-osx/c11dc7d435d10876918e98386e9db84bb96a030d85b832a4f86d23cd296c2b1c/9.4.7/bin"
libdir     = "/Users/espennoreng/Desktop/pacman/pacman/.stack-work/install/x86_64-osx/c11dc7d435d10876918e98386e9db84bb96a030d85b832a4f86d23cd296c2b1c/9.4.7/lib/x86_64-osx-ghc-9.4.7/gloss-pacman-0.1.0.0-6gObUl6lFi8H0cIkZlmEf8-gloss-counter"
dynlibdir  = "/Users/espennoreng/Desktop/pacman/pacman/.stack-work/install/x86_64-osx/c11dc7d435d10876918e98386e9db84bb96a030d85b832a4f86d23cd296c2b1c/9.4.7/lib/x86_64-osx-ghc-9.4.7"
datadir    = "/Users/espennoreng/Desktop/pacman/pacman/.stack-work/install/x86_64-osx/c11dc7d435d10876918e98386e9db84bb96a030d85b832a4f86d23cd296c2b1c/9.4.7/share/x86_64-osx-ghc-9.4.7/gloss-pacman-0.1.0.0"
libexecdir = "/Users/espennoreng/Desktop/pacman/pacman/.stack-work/install/x86_64-osx/c11dc7d435d10876918e98386e9db84bb96a030d85b832a4f86d23cd296c2b1c/9.4.7/libexec/x86_64-osx-ghc-9.4.7/gloss-pacman-0.1.0.0"
sysconfdir = "/Users/espennoreng/Desktop/pacman/pacman/.stack-work/install/x86_64-osx/c11dc7d435d10876918e98386e9db84bb96a030d85b832a4f86d23cd296c2b1c/9.4.7/etc"

getBinDir     = catchIO (getEnv "gloss_pacman_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "gloss_pacman_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "gloss_pacman_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "gloss_pacman_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gloss_pacman_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gloss_pacman_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
