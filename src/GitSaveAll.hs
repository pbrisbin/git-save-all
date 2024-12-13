{-# LANGUAGE QuasiQuotes #-}

module GitSaveAll
  ( main
  ) where

import Prelude

import Conduit
import Data.Bifunctor (bimap)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.These
import GitSaveAll.Branch
import GitSaveAll.BranchState
import GitSaveAll.Git
import Path
import Path.IO
import System.FilePath (dropTrailingPathSeparator)

code :: Path Abs Dir
code = [absdir|/home/patrick/code|]

includeOrgs :: [String]
includeOrgs =
  [ "pbrisbin"
  , "RenaissancePlace"
  , "restyled-io"
  , "archlinux-downgrade"
  , "freckle"
  ]

defaultRemote :: String
defaultRemote = "origin"

excludeRefs :: [String]
excludeRefs = ["HEAD", "main", "master", "develop"]

main :: IO ()
main = do
  (orgs, _) <- listDir code

  runConduit
    $ yieldMany orgs
    .| filterC includeOrg
    .| concatMapMC (fmap fst . listDir)
    .| sourceBranches
    .| filterC includeBranch
    .| processRepoBranch defaultRemote
    .| filterC (not . isInSyncOrBehind)
    .| mapM_C print
 where
  includeOrg :: Path Abs Dir -> Bool
  includeOrg =
    (`elem` includeOrgs)
      . dropTrailingPathSeparator
      . toFilePath
      . dirname

  includeBranch :: RepoBranch -> Bool
  includeBranch rb = case rb.branch of
    This a -> a `notElem` excludeRefs
    That {} -> False -- don't care about remote only
    These a _ -> a `notElem` excludeRefs

data RepoBranch = RepoBranch
  { repo :: Path Abs Dir
  , branch :: These String String
  }
  deriving stock Show

sourceBranches :: MonadIO m => ConduitT (Path Abs Dir) RepoBranch m ()
sourceBranches = awaitForever $ \repo -> do
  withFetchedRemote repo defaultRemote $ do
    bs <- branchListAll repo
    yieldMany
      $ map (RepoBranch repo)
      $ uncurry pairup
      $ bimap sort sort
      $ partitionBranches defaultRemote
      $ mapMaybe parseBranch bs

processRepoBranch :: MonadIO m => String -> ConduitT RepoBranch BranchState m ()
processRepoBranch remote = awaitForever $ \rb ->
  case rb.branch of
    This a -> yield $ PushNeeded rb.repo a Nothing
    That {} -> pure ()
    These a _ -> yield =<< lift (getBranchState rb.repo remote a)

pairup :: Ord a => [a] -> [a] -> [These a a]
pairup [] as = That <$> as
pairup as [] = This <$> as
pairup l@(x : xs) r@(y : ys)
  | x == y = These x y : pairup xs ys
  | x < y = This x : pairup xs r
  | otherwise = That y : pairup l ys
