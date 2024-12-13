module GitSaveAll.Git
  ( fetch
  , branchListAll
  , revListCount
  ) where

import Prelude

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BSL8
import System.Process.Typed
import UnliftIO.Exception (handleAny)

fetch :: String -> IO Bool
fetch remote =
  handleAny (const $ pure False) $ True <$ readGit ["fetch", remote]

branchListAll :: IO [String]
branchListAll =
  map BSL8.unpack . BSL8.lines <$> readGit ["branch", "--list", "--all"]

revListCount :: String -> String -> IO Int
revListCount a b =
  read . BSL8.unpack <$> readGit ["rev-list", "--count", spec]
 where
  spec = a <> ".." <> b

readGit :: [String] -> IO ByteString
readGit args = fst <$> readProcess_ (proc "git" args)
