{-# LANGUAGE QuasiQuotes #-}

-- |
--
-- Module      : GitSaveAll.Git
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GitSaveAll.Git
  ( isGitRepo
  , isGitDirty
  , fetch
  , branchListAll
  , revListCount
  , push
  ) where

import Prelude

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Path
import Path.IO
import System.Process.Typed

isGitRepo :: MonadIO m => Path Abs Dir -> m Bool
isGitRepo repo = doesDirExist $ repo </> dotGit

isGitDirty :: MonadIO m => Path Abs Dir -> m Bool
isGitDirty repo =
  not . null . BSL8.lines
    <$> readGit repo ["status", "--porcelain", "--no-untracked-files"]

fetch :: MonadIO m => Path Abs Dir -> String -> m ()
fetch repo remote = void $ readGit repo ["fetch", remote]

branchListAll :: MonadIO m => Path Abs Dir -> m [String]
branchListAll repo =
  map BSL8.unpack . BSL8.lines <$> readGit repo ["branch", "--list", "--all"]

revListCount :: MonadIO m => Path Abs Dir -> String -> String -> m Int
revListCount repo a b =
  read . BSL8.unpack <$> readGit repo ["rev-list", "--count", spec]
 where
  spec = a <> ".." <> b

push :: MonadIO m => Path Abs Dir -> String -> String -> m ()
push repo remote branch = runGit repo ["push", "--quiet", "-u", remote, branch]

readGit :: MonadIO m => Path Abs Dir -> [String] -> m ByteString
readGit repo args =
  fst
    <$> readProcess_ (proc "git" $ ["-C", toFilePath repo] <> args)

runGit :: MonadIO m => Path Abs Dir -> [String] -> m ()
runGit repo args =
  runProcess_ (proc "git" $ ["--git-dir", toFilePath $ repo </> dotGit] <> args)

dotGit :: Path Rel Dir
dotGit = [reldir|.git|]
