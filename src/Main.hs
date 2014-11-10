{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-------------------------------------------------------------------------------
-- |
-- Module     : Main
-- Copyright  : (c) 2014 Paulo Tanimoto
-- License    : BSD3
--
-- Maintainer : Paulo Tanimoto <ptanimoto@gmail.com>
--
-------------------------------------------------------------------------------

module Main
  ( main
  ) where


-------------------------------------------------------------------------------

import           WatchIt                   (defaultMain)


-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain
