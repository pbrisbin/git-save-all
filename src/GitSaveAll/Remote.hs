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
