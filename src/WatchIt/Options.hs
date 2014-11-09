{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-------------------------------------------------------------------------------
-- |
-- Module     : WatchIt.Options
-- Copyright  : (c) 2014 Paulo Tanimoto
-- License    : BSD3
--
-- Maintainer : Paulo Tanimoto <ptanimoto@gmail.com>
--
-------------------------------------------------------------------------------

module WatchIt.Options
  ( Options (..)
  , options
  , fullOptions
  ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import           Options.Applicative


-------------------------------------------------------------------------------
-- Data Types
-------------------------------------------------------------------------------

data Options = Options
  { optionsPath :: String
  , optionsCmd :: String
  }
  deriving (Eq, Show)


-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

options :: Parser Options
options = Options
  <$> strOption
      ( long "path"
     <> metavar "PATH"
     <> help "Directory to watch")
  <*> strOption
      ( long "command"
     <> metavar "COMMAND"
     <> help "Command to run")


fullOptions :: ParserInfo Options
fullOptions =
  info (helper <*> options)
    ( fullDesc
   <> progDesc "Run a COMMAND whenever there's a change in directory PATH"
   <> header "watchit - watch a directory and run a command on changes" )
