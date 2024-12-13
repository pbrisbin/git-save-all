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
import Data.String (IsString (..))
import Data.Text.Escaped
import Data.Text.IO qualified as T
import Data.These
import GitSaveAll.BranchState
import GitSaveAll.Git
import GitSaveAll.Options
import GitSaveAll.RepoBranch
import Path
import Path.IO
import System.FilePath (dropTrailingPathSeparator)

main :: IO ()
main = run =<< parseOptions

run :: Options -> IO ()
run options = do
  r <- terminalRenderer
  cwd <- getCurrentDir

  let
    toRepoBase :: Path Abs Dir -> Escaped
    toRepoBase x =
      fromString
        $ dropTrailingPathSeparator
        $ maybe (toFilePath x) toFilePath
        $ stripProperPrefix cwd x

    onRepoException :: RepoException -> IO ()
    onRepoException re =
      T.putStrLn
        $ r
        $ red "! "
        <> cyan (toRepoBase re.repo)
        <> red (" could not fetch " <> fromString options.remote)

    onBranchState :: BranchState -> IO ()
    onBranchState = \case
      InSyncOrBehind repo branch ->
        unless options.quiet $ do
          T.putStrLn
            $ r
            $ green "✓ "
            <> cyan (toRepoBase repo)
            <> "@"
            <> magenta (fromString branch)
            <> green " is in sync or behind"
      PushNeeded repo branch _ -> do
        T.putStrLn
          $ r
          $ yellow "✗ "
          <> cyan (toRepoBase repo)
          <> "@"
          <> magenta (fromString branch)
          <> yellow " needs to be pushed"
        when options.push $ push repo options.remote branch
      SyncNeeded repo branch _ _ ->
        T.putStrLn
          $ r
          $ red "! "
          <> cyan (toRepoBase repo)
          <> "@"
          <> magenta (fromString branch)
          <> red " needs to be force-pushed"

  let repos = maybe (pure cwd) (fmap (cwd </>)) $ nonEmpty options.repos

  runConduit
    $ yieldMany repos
    .| filterMC isGitRepo
    .| awaitForever (sourceRepoBranches options.remote)
    .| awaitForever (either (liftIO . onRepoException) yield)
    .| filterC includeBranch
    .| awaitForever (sourceBranchState options.remote)
    .| mapM_C onBranchState
 where
  includeBranch :: RepoBranch -> Bool
  includeBranch rb = case rb.branch of
    This a -> a `notElem` options.exclude
    That {} -> False -- don't care about remote only
    These a _ -> a `notElem` options.exclude
