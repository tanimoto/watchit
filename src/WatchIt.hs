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
  , watchIt
  ) where


-------------------------------------------------------------------------------

import           WatchIt.Options
import           WatchIt.Types

import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forever, void)

import           Data.Pool                 (Pool (..), createPool, tryWithResource)
import           Data.Streaming.Process    (Inherited (..), shell, streamingProcess,
                                           waitForStreamingProcess)
import qualified Data.Text                 as Text

import qualified Filesystem.Path.CurrentOS as FS

import           Options.Applicative       (execParser)

import           System.FSNotify           (eventPath, watchDir, watchTree,
                                           withManager)


-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = do
  options <- execParser infoOptions
  watchIt $ parseConfig options


-------------------------------------------------------------------------------

parseConfig :: Options -> Config
parseConfig options = Config
  { configPath = withDef configPath optionsPath FS.decodeString
  , configFilter = withDef configFilter optionsExt
                   (flip FS.hasExtension . Text.pack)
  , configAction = withDef configAction optionsCmd (const . run)
  , configNumJobs = withDef configNumJobs optionsNumJobs id
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
  let longDelay = 12 * 3600 * 10000  -- maxBound

  -- Watch it
  putStrLn "watchit started..."
  withManager $ \man -> do
    void $ watch man path
      filterEvent
      handleEvent
    forever $ threadDelay longDelay


--------------------------------------------------------------------------------

createWorkerPool :: Int -> IO (Pool ())
createWorkerPool stripes =
  createPool
    (return ())
    (const $ return ())
    stripes timeLeftOpen numPerStripe
  where
  timeLeftOpen = 1
  numPerStripe = 1


withPool :: Pool a -> (FS.FilePath -> IO ()) -> FS.FilePath -> IO ()
withPool pool f file = do
  void $ tryWithResource pool $ const $ f file


--------------------------------------------------------------------------------

run :: String -> IO ()
run cmd = do
  putStrLn $ replicate 72 '-'
  (Inherited, Inherited, Inherited, handle) <-
    streamingProcess (shell cmd)
  waitForStreamingProcess handle >>= print
