module GitSaveAll.BranchState
  ( BranchState (..)
  , getBranchState
  , sourceBranchState
  ) where

import Prelude

import Conduit
import Data.These
import GitSaveAll.Git (revListCount)
import GitSaveAll.RepoBranch
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

sourceBranchState
  :: MonadIO m
  => String
  -> RepoBranch
  -> ConduitT RepoBranch BranchState m ()
sourceBranchState remote rb =
  case rb.branch of
    This a -> yield $ PushNeeded rb.repo a Nothing
    That {} -> pure ()
    These a _ -> yield =<< lift (getBranchState rb.repo remote a)
