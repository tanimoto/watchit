{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Bench
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

import           Criterion.Main

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain benchmarks

-------------------------------------------------------------------------------
-- Benchmarks
-------------------------------------------------------------------------------

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "Benchmarks"
    [ bench "Factorial 20" $ nf fact 20
    , bench "Factorial 30" $ nf fact 30
    ]
  ]
