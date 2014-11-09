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

import           Data.Pool                 (Pool (..), createPool, tryWithResource)
import           Data.Streaming.Process    (Inherited (..), shell, streamingProcess,
                                           waitForStreamingProcess)

import           Filesystem.Path.CurrentOS as FS

import           Options.Applicative       (execParser)

import           System.FSNotify           (Event (..), eventPath, watchTree,
                                           withManager)


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

  -- Worker Pool
  pool <- createWorkerPool

  -- Watch it
  putStrLn "watchit started..."
  withManager $ \man -> do
    void $ watchTree man path
      filterEvent
      (handleEvent pool cmd)
    forever $ threadDelay longDelay
  where
  longDelay = 12 * 3600 * 10000  -- maxBound


filterEvent :: Event -> Bool
filterEvent event = do
  let file = eventPath event
  let ext = "hs"
  hasExtension file ext


handleEvent :: Pool a -> String -> Event -> IO ()
handleEvent pool cmd _ = do
  void $ tryWithResource pool $ const
       $ run cmd


createWorkerPool :: IO (Pool ())
createWorkerPool =
  createPool
    (return ())
    (const $ return ())
    stripes timeLeftOpen resPerStripe
  where
  stripes = 1
  timeLeftOpen = 1
  resPerStripe = 1


run :: String -> IO ()
run cmd = do
  putStrLn ""
  putStrLn "------------------------------------------------------------------------"
  (Inherited, Inherited, Inherited, handle) <-
    streamingProcess (shell cmd)
  status <- waitForStreamingProcess handle
  print status
