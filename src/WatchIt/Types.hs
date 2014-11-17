{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- |
-- Module     : WatchIt.Types
-- Copyright  : (c) 2014 Paulo Tanimoto
-- License    : BSD3
--
-- Maintainer : Paulo Tanimoto <ptanimoto@gmail.com>
--
-------------------------------------------------------------------------------

module WatchIt.Types
  ( Config (..)
  , defaultConfig
  ) where

-------------------------------------------------------------------------------

import qualified Filesystem.Path.CurrentOS as FS


-------------------------------------------------------------------------------

data Config = Config
  { configPath :: FS.FilePath
  , configFilter :: FS.FilePath -> Bool
  , configAction :: FS.FilePath -> IO ()
  , configForce :: Bool
  , configNumJobs :: Int
  , configRecur :: Bool
  }


-------------------------------------------------------------------------------

defaultConfig :: Config
defaultConfig = Config
  { configPath = "."
  , configFilter = const True
  , configAction = printFile
  , configForce = False
  , configNumJobs = 1
  , configRecur = True
  }


printFile :: FS.FilePath -> IO ()
printFile = putStrLn . FS.encodeString
