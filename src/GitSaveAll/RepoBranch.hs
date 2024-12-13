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
  , sourceRepoBranches
  ) where

import Prelude

import Conduit
import Data.Bifunctor (bimap)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.These
import GitSaveAll.Branch
import GitSaveAll.Git
import Path

data RepoBranch = RepoBranch
  { repo :: Path Abs Dir
  , branch :: These String String
  }
  deriving stock Show

sourceRepoBranches
  :: MonadIO m => String -> Path Abs Dir -> ConduitT (Path Abs Dir) RepoBranch m ()
sourceRepoBranches remote repo = do
  withFetchedRemote repo remote $ do
    bs <- branchListAll repo
    yieldMany
      $ map (RepoBranch repo)
      $ uncurry pairup
      $ bimap sort sort
      $ partitionBranches remote
      $ mapMaybe parseBranch bs

pairup :: Ord a => [a] -> [a] -> [These a a]
pairup [] as = That <$> as
pairup as [] = This <$> as
pairup l@(x : xs) r@(y : ys)
  | x == y = These x y : pairup xs ys
  | x < y = This x : pairup xs r
  | otherwise = That y : pairup l ys
