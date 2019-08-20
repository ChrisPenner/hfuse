-----------------------------------------------------------------------------
-- |
-- Module      :  System.Fuse
-- Copyright   :  (c) Jérémy Bobbio, Taru Karttunen
-- License     :  BSD-style
--
-- Maintainer  :  Montez Fitzpatrick
-- Stability   :  experimental
-- Portability :  GHC 6.4-7.8.2
--
-- A binding for the FUSE (Filesystem in USErspace) library
-- (<http://fuse.sourceforge.net/>), which allows filesystems to be implemented
-- as userspace processes.
--
-- The binding tries to follow as much as possible current Haskell POSIX
-- interface in "System.Posix.Files" and "System.Posix.Directory".
--
-- FUSE uses POSIX threads, so any Haskell application using this library must
-- be linked against a threaded runtime system (eg. using the @threaded@ GHC
-- option).
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
module System.Fuse
    ( -- * Using FUSE

      -- $intro

      module Foreign.C.Error
    , FuseOperations(..)
    , defaultFuseOps
    , fuseMain -- :: FuseOperations fh -> (Exception -> IO Errno) -> IO ()
    , fuseRun -- :: String -> [String] -> FuseOperations fh -> (Exception -> IO Errno) -> IO ()

    , fuseMainInline -- :: FuseOperations fh -> (Exception -> IO Errno) -> IO ()
    , fuseRunInline -- :: String -> [String] -> FuseOperations fh -> (Exception -> IO Errno) -> IO ()
    , defaultExceptionHandler -- :: Exception -> IO Errno
      -- * Operations datatypes
    , FileStat(..)
    , EntryType(..)
    , FileSystemStats(..)
    , SyncType(..)
      -- * FUSE Context
    , getFuseContext -- :: IO FuseContext
    , FuseContext(fuseCtxUserID, fuseCtxGroupID, fuseCtxProcessID)
      -- * File modes
    , entryTypeToFileMode -- :: EntryType -> FileMode
    , fileModeToEntryType -- :: FileMode -> EntryType
    , OpenMode(..)
    , OpenFileFlags(..)
    , intersectFileModes -- :: FileMode
    , unionFileModes -- :: FileMode
    ) where


import System.Fuse.Bindings
import System.Fuse.Main
import Foreign.C.Error
