{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-------------------------------------------------------------------------------
-- |
-- Module     : WatchIt
-- Copyright  : (c) 2014 Paulo Tanimoto
-- License    : BSD3
--
-- Maintainer : Paulo Tanimoto <ptanimoto@gmail.com>
--
-------------------------------------------------------------------------------

module WatchIt
  ( defaultMain
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import           WatchIt.Options

import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forever, void)

import           Data.Streaming.Process    (Inherited (..), shell, streamingProcess,
                                           waitForStreamingProcess)

import           Filesystem.Path.CurrentOS as FS

import           Options.Applicative       (execParser)

import           System.FSNotify           (eventPath, watchTree, withManager)


-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = do
  execParser fullOptions >>= watchIt


watchIt :: Options -> IO ()
watchIt opts = do
  let path = decodeString $ optionsPath opts
  let cmd = optionsCmd opts
  withManager $ \man -> do
    void $ watchTree man
      path
      filterEvent
      (const $ run cmd)
    forever $ threadDelay longDelay
  where
  ext = "hs"
  filterEvent event = do
    let file = eventPath event
    hasExtension file ext
  longDelay = 12 * 3600 * 10000  -- maxBound


run :: String -> IO ()
run cmd = do
  (Inherited, Inherited, Inherited, handle) <-
    streamingProcess (shell cmd)
  void $ waitForStreamingProcess handle
