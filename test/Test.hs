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
-- Imports
-------------------------------------------------------------------------------

import           WatchIt

import           Test.Tasty
import qualified Test.Tasty.SmallCheck     as SC
import qualified Test.Tasty.QuickCheck     as QC
import           Test.Tasty.HUnit

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testSuite

-------------------------------------------------------------------------------
-- Test Suite
-------------------------------------------------------------------------------

testSuite :: TestTree
testSuite = testGroup "Test Suite"
  [ props
  , units
  ]

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

props :: TestTree
props = testGroup "Properties"
  [ QC.testProperty "id" $
      \n -> (n::Int) == id n
  ]

-------------------------------------------------------------------------------
-- Unit Tests
-------------------------------------------------------------------------------

units :: TestTree
units = testGroup "Unit Tests"
  [ testCase "Factorial of 0 is 1" $
    fact 0 == 1 @?= True
  , testCase "Factorial of 5 is 120" $
    fact 5 == 120 @?= True
  ]
