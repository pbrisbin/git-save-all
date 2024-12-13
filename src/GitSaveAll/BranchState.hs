module GitSaveAll.BranchState
  ( BranchState (..)
  , getBranchState
  , isInSyncOrBehind
  ) where

import Prelude

import Control.Monad.IO.Class (MonadIO (..))
import GitSaveAll.Git (revListCount)
import Path

data BranchState
  = InSyncOrBehind (Path Abs Dir) String
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
    (0, _) -> InSyncOrBehind repo branch
    (a, 0) -> PushNeeded repo branch (Just a) -- ahead
    (a, b) -> SyncNeeded repo branch a b

  rbranch = remote <> "/" <> branch

isInSyncOrBehind :: BranchState -> Bool
isInSyncOrBehind = \case
  InSyncOrBehind {} -> True
  PushNeeded {} -> False
  SyncNeeded {} -> False
