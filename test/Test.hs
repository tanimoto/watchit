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
import qualified Filesystem                as FS

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
  [ props
  , units
  ]

-------------------------------------------------------------------------------

props :: TestTree
props = testGroup "Properties"
  [
  ]

-------------------------------------------------------------------------------

units :: TestTree
units = testGroup "Unit Tests"
  [ testCase "Watching should notify of a file change" $ do
      let path = "tmp"
      mvar <- newEmptyMVar

      let config = defaultConfig
                   { configPath = path
                   , configAction = \_ -> putMVar mvar ()
                   }
      watcher <- async $ watchIt config

      FS.createDirectory True path
      FS.writeFile "tmp/test" B.empty

      threadDelay (3*1000*1000)

      actual <- tryReadMVar mvar
      cancel watcher

      assertEqual "" (Just ()) actual
  ]
