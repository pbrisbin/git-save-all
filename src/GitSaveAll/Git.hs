{-# LANGUAGE QuasiQuotes #-}

module GitSaveAll.Git
  ( withFetchedRemote
  , branchListAll
  , revListCount
  ) where

import Prelude

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Path
import Path.IO
import System.Process.Typed
import UnliftIO.Exception (handleAny)

withFetchedRemote :: MonadIO m => Path Abs Dir -> String -> m () -> m ()
withFetchedRemote repo remote f = do
  isGit <- doesDirExist $ repo </> dotGit

  when isGit $ do
    fetched <-
      liftIO
        $ handleAny (const $ pure False)
        $ True
        <$ readGit repo ["fetch", remote]

    when fetched f

branchListAll :: MonadIO m => Path Abs Dir -> m [String]
branchListAll repo =
  map BSL8.unpack . BSL8.lines <$> readGit repo ["branch", "--list", "--all"]

revListCount :: MonadIO m => Path Abs Dir -> String -> String -> m Int
revListCount repo a b =
  read . BSL8.unpack <$> readGit repo ["rev-list", "--count", spec]
 where
  spec = a <> ".." <> b

readGit :: MonadIO m => Path Abs Dir -> [String] -> m ByteString
readGit repo args =
  fst
    <$> readProcess_ (proc "git" $ ["--git-dir", toFilePath $ repo </> dotGit] <> args)

dotGit :: Path Rel Dir
dotGit = [reldir|.git|]
