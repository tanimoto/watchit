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
  , defaultOptions
  , parseOptions
  , infoOptions
  ) where

-------------------------------------------------------------------------------

import           Options.Applicative


-------------------------------------------------------------------------------

data Options = Options
  { optionsPath :: Maybe String
  , optionsExt :: Maybe String
  , optionsCmd :: Maybe String
  , optionsForce :: Maybe Bool
  , optionsNumJobs :: Maybe Int
  , optionsNotRec :: Maybe Bool
  }
  deriving (Eq, Show)


-------------------------------------------------------------------------------

defaultOptions :: Options
defaultOptions = Options
  { optionsPath = Nothing
  , optionsExt = Nothing
  , optionsCmd = Nothing
  , optionsForce = Nothing
  , optionsNumJobs = Nothing
  , optionsNotRec = Nothing
  }


parseOptions :: Parser Options
parseOptions = Options
  <$> optional (strOption
      ( long "path"
     <> metavar "PATH"
     <> help "Directory to watch"))
  <*> optional (strOption
      ( long "extension"
     <> metavar "EXTENSION"
     <> help "File extension to watch"))
  <*> optional (strOption
      ( long "command"
     <> metavar "COMMAND"
     <> help "Command to run"))
  <*> optional (switch
      ( long "force"
     <> short 'f'
     <> help "Force command to run right away"))
  <*> optional (option auto
      ( long "num-jobs"
     <> short 'j'
     <> metavar "JOBS"
     <> help "Number of concurrent jobs"))
  <*> optional (switch
      ( long "not-recursive"
     <> help "Do not watch directory recursively"))


infoOptions :: ParserInfo Options
infoOptions =
  info (helper <*> parseOptions)
    ( fullDesc
   <> progDesc "Watch a directory and run a command whenever a file changes"
   <> header "watchit" )
