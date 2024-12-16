-- |
--
-- Module      : GitSaveAll.RepoBranch
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GitSaveAll.RepoBranch
  ( RepoBranch (..)
  , RepoException (..)
  , ExitCodeException (..)
  , sourceRepoBranches
  ) where

import Prelude

import Conduit
import Control.Monad (unless, when)
import Data.Bifunctor (bimap)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.These
import GitSaveAll.Branch
import GitSaveAll.Git
import Path
import System.Process.Typed (ExitCodeException (..))
import UnliftIO.Exception (Exception, catch, throwIO, try)

data RepoBranch = RepoBranch
  { repo :: Path Abs Dir
  , branch :: These String String
  }
  deriving stock Show

data RepoException
  = NotGit (Path Abs Dir)
  | GitDirty (Path Abs Dir)
  | GitFetchError (Path Abs Dir) ExitCodeException
  deriving stock Show
  deriving anyclass Exception

sourceRepoBranches
  :: MonadIO m
  => String
  -> Path Abs Dir
  -> ConduitT (Path Abs Dir) (Either RepoException RepoBranch) m ()
sourceRepoBranches remote repo = do
  result <- liftIO $ try $ ensureCleanFetchedRepo repo remote

  case result of
    Left ex -> yield $ Left ex
    Right () -> do
      bs <- branchListAll repo
      yieldMany
        $ map (Right . RepoBranch repo)
        $ uncurry pairup
        $ bimap sort sort
        $ partitionBranches remote
        $ mapMaybe parseBranch bs

ensureCleanFetchedRepo :: Path Abs Dir -> String -> IO ()
ensureCleanFetchedRepo repo remote = do
  isGit <- isGitRepo repo
  unless isGit $ throwIO $ NotGit repo

  isDirty <- isGitDirty repo
  when isDirty $ throwIO $ GitDirty repo

  fetch repo remote `catch` \ex -> throwIO (GitFetchError repo ex)

pairup :: Ord a => [a] -> [a] -> [These a a]
pairup [] as = That <$> as
pairup as [] = This <$> as
pairup l@(x : xs) r@(y : ys)
  | x == y = These x y : pairup xs ys
  | x < y = This x : pairup xs r
  | otherwise = That y : pairup l ys
