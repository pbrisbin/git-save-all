-- |
--
-- Module      : GitSaveAll
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GitSaveAll
  ( main
  ) where

import Prelude

import Conduit
import Control.Monad (unless, when)
import Data.List.NonEmpty (nonEmpty)
import Data.These
import GitSaveAll.BranchState
import GitSaveAll.Git (push)
import GitSaveAll.Options
import GitSaveAll.RepoBranch
import Path
import Path.IO
import System.FilePath (dropTrailingPathSeparator)

main :: IO ()
main = run =<< parseOptions

run :: Options -> IO ()
run options = do
  cwd <- getCurrentDir

  let
    toRepoBase :: Path Abs Dir -> String
    toRepoBase x =
      dropTrailingPathSeparator
        $ maybe (toFilePath x) toFilePath
        $ stripProperPrefix cwd x

    onBranchState :: BranchState -> IO ()
    onBranchState = \case
      InSyncOrBehind repo branch ->
        unless options.quiet $ do
          putStrLn $ "✓ " <> toRepoBase repo <> "@" <> branch <> " is in sync or behind"
      PushNeeded repo branch _ -> do
        putStrLn $ "✗ " <> toRepoBase repo <> "@" <> branch <> " needs to be pushed"
        when options.push $ push repo options.remote branch
      SyncNeeded repo branch _ _ ->
        putStrLn $ "! "
          <> toRepoBase repo
          <> "@"
          <> branch
          <> " needs to be force-pushed"

  let repos = maybe (pure cwd) (fmap (cwd </>)) $ nonEmpty options.repos

  runConduit
    $ yieldMany repos
    .| awaitForever (sourceRepoBranches options.remote)
    .| filterC includeBranch
    .| awaitForever (sourceBranchState options.remote)
    .| mapM_C onBranchState
 where
  includeBranch :: RepoBranch -> Bool
  includeBranch rb = case rb.branch of
    This a -> a `notElem` options.exclude
    That {} -> False -- don't care about remote only
    These a _ -> a `notElem` options.exclude
