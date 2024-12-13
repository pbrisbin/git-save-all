-- |
--
-- Module      : GitSaveAll.Remote
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GitSaveAll.Remote
  ( Remote
  , toString
  ) where

import Prelude

import Data.String (IsString (..))

data Remote
  = Origin
  | Other String

instance IsString Remote where
  fromString = \case
    "origin" -> Origin
    x -> Other x

toString :: Remote -> String
toString = \case
  Origin -> "origin"
  Other x -> x
