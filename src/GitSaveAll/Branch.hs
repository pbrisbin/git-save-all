-- |
--
-- Module      : GitSaveAll.Branch
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module GitSaveAll.Branch
  ( Branch (..)
  , parseBranch
  , partitionBranches
  ) where

import Prelude

import Control.Applicative ((<|>))
import Data.List (foldl')
import Data.List.NonEmpty (nonEmpty)
import Data.List.NonEmpty qualified as NE
import Text.ParserCombinators.ReadP

data Branch
  = Local String
  | Remote String String
  deriving stock Show

parseBranch :: String -> Maybe Branch
parseBranch = parse $ localP <|> remoteP

parse :: ReadP a -> String -> Maybe a
parse p = fmap (fst . NE.last) . nonEmpty . readP_to_S (p <* eof)

-- | @* main@
-- | @  pb/x@
localP :: ReadP Branch
localP = do
  optional $ char '*'
  skipSpaces *> fmap Local nonSpace

-- |
--
-- * @  remotes/origin/HEAD -> origin/main@
-- * @  remotes/origin/foo@
remoteP :: ReadP Branch
remoteP = do
  r <- skipSpaces *> string "remotes/" *> manyTill anyChar (char '/')
  b <- nonSpace
  Remote r b <$ optional (char ' ' *> string "->" *> char ' ' *> many1 anyChar)

nonSpace :: ReadP [Char]
nonSpace = many1 $ satisfy (/= ' ')

anyChar :: ReadP Char
anyChar = satisfy $ const True

partitionBranches :: String -> [Branch] -> ([String], [String])
partitionBranches remote = foldl' go ([], [])
 where
  go acc@(ls, rs) = \case
    Local b -> (ls <> [b], rs)
    Remote o b | o == remote -> (ls, rs <> [b])
    Remote {} -> acc -- ignore other remotes
