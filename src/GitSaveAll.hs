{-# LANGUAGE QuasiQuotes #-}

module GitSaveAll
  ( main
  ) where

import Prelude

import Control.Monad (when)
import Data.Foldable (for_)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.These
import GitSaveAll.Branch
import GitSaveAll.Git
import Path
import Path.IO
import System.FilePath (dropTrailingPathSeparator)

code :: Path Abs Dir
code = [absdir|/home/patrick/code|]

defaultRemote :: String
defaultRemote = "origin"

excludeBranches :: [String]
excludeBranches = ["main", "master", "develop"]

includeOrgs :: [String]
includeOrgs =
  [ "pbrisbin"
  , "RenaissancePlace"
  , "restyled-io"
  , "archlinux-downgrade"
  , "freckle"
  ]

main :: IO ()
main = do
  (orgs, _) <- listDir code

  for_ orgs $ \org -> do
    let name = dropTrailingPathSeparator $ toFilePath $ dirname org

    when (name `elem` includeOrgs) $ do
      (repos, _) <- listDir org

      for_ repos $ \repo -> do
        withCurrentDir repo $ do
          isGit <- doesDirExist [reldir|.git|]
          when isGit $ do
            fetched <- fetch defaultRemote
            when fetched $ processRepo repo

processRepo :: Path Abs Dir -> IO ()
processRepo repo = do
  bs <- branchListAll
  base <- dropTrailingPathSeparator . toFilePath <$> stripProperPrefix code repo

  let
    (locals, remotes) =
      partitionBranches defaultRemote $ mapMaybe parseBranch bs

    pairs =
      mapMaybe
        ( \case
            This a | a `elem` excludeBranches -> Nothing
            This a -> Just (a, Nothing)
            That {} -> Nothing
            These a _ | a `elem` excludeBranches -> Nothing
            These a b -> Just (a, Just b)
        )
        $ pairup (sort locals) (sort remotes)

  for_ pairs $ \(local, mRemote) -> do
    getBranchState local mRemote >>= \case
      UpToDate -> pure ()
      BehindRemote {} -> pure ()
      x -> putStrLn $ unwords [base, local, show x]

data BranchState
  = RemoteDoesNotExist
  | AheadOfRemote Int
  | BehindRemote Int
  | NeedsForcePush Int Int
  | UpToDate
  deriving stock Show

getBranchState :: String -> Maybe String -> IO BranchState
getBranchState local = \case
  Nothing -> pure RemoteDoesNotExist
  Just remote -> do
    let qremote = defaultRemote <> "/" <> remote

    ahead <- revListCount qremote local
    behind <- revListCount local qremote

    pure $ case (ahead, behind) of
      (0, 0) -> UpToDate
      (0, b) -> BehindRemote b
      (a, 0) -> AheadOfRemote a
      (a, b) -> NeedsForcePush a b

pairup :: Ord a => [a] -> [a] -> [These a a]
pairup [] as = That <$> as
pairup as [] = This <$> as
pairup l@(x : xs) r@(y : ys)
  | x == y = These x y : pairup xs ys
  | x < y = This x : pairup xs r
  | otherwise = That y : pairup l ys
