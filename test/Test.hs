{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Test
-- Copyright  : (c) 2014 Paulo Tanimoto
-- License    : BSD3
--
-- Maintainer : Paulo Tanimoto <ptanimoto@gmail.com>
--
-------------------------------------------------------------------------------

module Main where

-------------------------------------------------------------------------------

import           WatchIt
import           WatchIt.Types

import qualified Data.ByteString           as B

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception         (bracket)

import qualified Filesystem                as FS
import qualified Filesystem.Path.CurrentOS as FS

import           Test.Tasty                as Tasty
-- import qualified Test.Tasty.SmallCheck     as SC
-- import qualified Test.Tasty.QuickCheck     as QC
import           Test.Tasty.HUnit

-------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMain testSuite

-------------------------------------------------------------------------------

testSuite :: TestTree
testSuite = testGroup "Test Suite"
  [ properties
  , unitTests
  , integrationTests
  ]

-------------------------------------------------------------------------------

properties :: TestTree
properties = testGroup "Properties"
  [
  ]

-------------------------------------------------------------------------------

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [
  ]

-------------------------------------------------------------------------------

integrationTests :: TestTree
integrationTests = testGroup "Integration Tests"
  [ testWatchFileAdded
  ]


testWatchFileAdded :: TestTree
testWatchFileAdded =
  testCase "Watching should receive notification of a file change" $ do
    actual <- bracket acquire release between
    assertEqual "" (Just ()) actual
  where
  acquire = do
    let path = "dist/tmp"
    FS.createTree path
    return path
  release path = do
    FS.removeTree path
  between path = do
    mvar <- newEmptyMVar
    let config = defaultConfig
         { configPath = path
         , configAction = \_ -> putMVar mvar ()
         }
    withAsync (watchIt config) $ \_ -> do
      FS.writeFile (path FS.</> "tmp") B.empty
      threadDelay timeoutDelay
      tryReadMVar mvar
  timeoutDelay = 3*1000*1000
