{-# LANGUAGE QuasiQuotes #-}

module GitSaveAll.Options
  ( Options (..)
  , parseOptions
  ) where

import Prelude

import Path

data Options = Options
  { code :: Path Abs Dir
  , includeOrgs :: [String]
  , excludeRefs :: [String]
  , remote :: String
  }

parseOptions :: IO Options
parseOptions =
  pure
    Options
      { code = [absdir|/home/patrick/code|]
      , includeOrgs =
          [ "pbrisbin"
          , "RenaissancePlace"
          , "restyled-io"
          , "archlinux-downgrade"
          , "freckle"
          ]
      , excludeRefs = ["HEAD", "main", "master", "develop"]
      , remote = "origin"
      }
