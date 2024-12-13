module GitSaveAll.Options
  ( Options (..)
  , parseOptions
  ) where

import Prelude

import Data.Bifunctor (first)
import Options.Applicative
import Path
import UnliftIO.Exception (displayException)

data Options = Options
  { remote :: String
  , exclude :: [String]
  , push :: Bool
  , quiet :: Bool
  , repos :: [Path Rel Dir]
  }

parseOptions :: IO Options
parseOptions = do
  execParser
    $ info (optionsParser <**> helper)
    $ fullDesc
    <> progDesc "Report, and optionally push, branches that are behind a remote"

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( mconcat
          [ short 'r'
          , long "remote-name"
          , help "The name of the remote to use"
          , metavar "REMOTE"
          , showDefault
          , value "origin"
          ]
      )
    <*> many
      ( strOption
          ( mconcat
              [ short 'x'
              , long "exclude"
              , help "Branches to exclude from processing"
              , metavar "BRANCH"
              ]
          )
      )
    <*> switch
      ( mconcat
          [ short 'p'
          , long "push"
          , help "Actually push branches found to be behind"
          ]
      )
    <*> switch
      ( mconcat
          [ short 'q'
          , long "quiet"
          , help "Don't report in-sync branches"
          ]
      )
    <*> many
      ( argument
          (eitherReader $ first displayException . parseRelDir)
          ( mconcat
              [ help "Git repository to operate on"
              , metavar "DIRECTORY"
              ]
          )
      )
