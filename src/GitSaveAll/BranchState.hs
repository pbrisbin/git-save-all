module GitSaveAll.BranchState
  ( BranchState (..)
  , getBranchState
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO (..))
import GitSaveAll.Git (revListCount)
import Path

data BranchState
  = InSyncOrBehind
  | PushNeeded (Path Abs Dir) String (Maybe Int)
  | SyncNeeded (Path Abs Dir) String Int Int
  deriving stock Show

getBranchState
  :: MonadIO m
  => Path Abs Dir
  -> String
  -- ^ Remote
  -> String
  -- ^ Local branch
  -> m BranchState
getBranchState repo remote branch = do
  go
    <$> revListCount repo rbranch branch
    <*> revListCount repo branch rbranch
 where
  go = curry $ \case
    (0, _) -> InSyncOrBehind
    (a, 0) -> PushNeeded repo branch (Just a) -- ahead
    (a, b) -> SyncNeeded repo branch a b

  rbranch = remote <> "/" <> branch
