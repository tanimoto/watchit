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
import qualified Data.Text                 as Text

import           Filesystem.Path.CurrentOS as FS

import           Options.Applicative       (execParser)

import           System.FSNotify           (eventPath, watchDir, watchTree,
                                           withManager)


-------------------------------------------------------------------------------
-- Data Types
-------------------------------------------------------------------------------

data Config = Config
  { configPath :: FS.FilePath
  , configFilter :: FS.FilePath -> Bool
  , configAction :: FS.FilePath -> IO ()
  , configNumJobs :: Int
  , configRecur :: Bool
  }


defaultConfig :: Config
defaultConfig = Config
  { configPath = "."
  , configFilter = const True
  , configAction = printFile
  , configNumJobs = 1
  , configRecur = True
  }


-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = do
  options <- execParser infoOptions
  watchIt $ parseConfig options


parseConfig :: Options -> Config
parseConfig options = Config
  { configPath = withDef configPath optionsPath decodeString
  , configFilter = withDef configFilter optionsExt
                   (flip hasExtension . Text.pack)
  , configAction = withDef configAction optionsCmd (const . run)
  , configNumJobs = withDef configNumJobs optionsNumJobs read
  , configRecur = withDef configRecur optionsNotRec not
  }
  where
  withDef conf opt f = maybe (conf defaultConfig) f (opt options)


watchIt :: Config -> IO ()
watchIt config = do
  -- Set up Config
  let path = configPath config
  let filterEvent = configFilter config . eventPath
  let numJobs = configNumJobs config
  pool <- createWorkerPool numJobs
  let handleEvent = withPool pool (configAction config) . eventPath
  let watch = if configRecur config then watchTree else watchDir

  -- Watch it
  putStrLn "watchit started..."
  withManager $ \man -> do
    void $ watch man path
      filterEvent
      handleEvent
    forever $ threadDelay longDelay
  where
  longDelay = 12 * 3600 * 10000  -- maxBound


withPool :: Pool a -> (FS.FilePath -> IO ()) -> FS.FilePath -> IO ()
withPool pool f file = do
  void $ tryWithResource pool (const $ f file)


createWorkerPool :: Int -> IO (Pool ())
createWorkerPool stripes =
  createPool
    (return ())
    (const $ return ())
    stripes timeLeftOpen numPerStripe
  where
  timeLeftOpen = 1
  numPerStripe = 1


run :: String -> IO ()
run cmd = do
  putStrLn ""
  putStrLn "------------------------------------------------------------------------"
  (Inherited, Inherited, Inherited, handle) <-
    streamingProcess (shell cmd)
  status <- waitForStreamingProcess handle
  print status


printFile :: FS.FilePath -> IO ()
printFile = putStrLn . encodeString
