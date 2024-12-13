module GitSaveAll
  ( main
  ) where

import Prelude

import Conduit
import Data.These
import GitSaveAll.BranchState
import GitSaveAll.Options
import GitSaveAll.RepoBranch
import Path
import Path.IO
import System.FilePath (dropTrailingPathSeparator)

main :: IO ()
main = do
  options <- parseOptions

  let
    includeOrg :: Path Abs Dir -> Bool
    includeOrg =
      (`elem` options.includeOrgs)
        . dropTrailingPathSeparator
        . toFilePath
        . dirname

    includeBranch :: RepoBranch -> Bool
    includeBranch rb = case rb.branch of
      This a -> a `notElem` options.excludeRefs
      That {} -> False -- don't care about remote only
      These a _ -> a `notElem` options.excludeRefs

    toRepoBase :: Path Abs Dir -> String
    toRepoBase x =
      dropTrailingPathSeparator
        $ maybe (toFilePath x) toFilePath
        $ stripProperPrefix options.code x

    onBranchState :: BranchState -> IO ()
    onBranchState = \case
      InSyncOrBehind {} -> pure ()
      PushNeeded repo branch _ ->
        putStrLn $ toRepoBase repo <> "@" <> branch <> " needs to be pushed"
      SyncNeeded repo branch _ _ ->
        putStrLn $ toRepoBase repo <> "@" <> branch <> " needs to be force-pushed"

  (orgs, _) <- listDir options.code

  runConduit
    $ yieldMany orgs
    .| filterC includeOrg
    .| concatMapMC (fmap fst . listDir)
    .| awaitForever (sourceRepoBranches options.remote)
    .| filterC includeBranch
    .| awaitForever (sourceBranchState options.remote)
    .| mapM_C onBranchState
