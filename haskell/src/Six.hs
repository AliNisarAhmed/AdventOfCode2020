{-# LANGUAGE OverloadedStrings #-}

module Six where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text, foldl')
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data GroupVotes = GroupVotes
  { size :: Int,
    votes :: Map Char Int
  }
  deriving (Show)

defaultGroups :: GroupVotes
defaultGroups =
  GroupVotes
    { size = 1,
      votes = M.empty
    }

emptySet :: S.Set Char
emptySet = S.empty

sixPartOne :: IO ()
sixPartOne = do
  contents <- T.splitOn "\n\n" <$> TIO.readFile "../data/6.txt"
  let s = sum $ S.size . accumulateInSet <$> contents
  print s
  return ()

sixPartTwo :: IO ()
sixPartTwo = do
  contents <- T.splitOn "\n\n" <$> TIO.readFile "../data/6.txt"
  let s = sum $ countVotesMatchingSize . accumulateInGroups <$> contents
  print s
  return ()

accumulateInGroups :: Text -> GroupVotes
accumulateInGroups = foldl' step defaultGroups
  where
    step (GroupVotes s m) x =
      if x == '\n'
        then GroupVotes (s + 1) m
        else GroupVotes s (M.insertWith (+) x 1 m)

countVotesMatchingSize :: GroupVotes -> Int
countVotesMatchingSize (GroupVotes s m) = M.size $ M.filter (== s) m

accumulateInSet :: Text -> S.Set Char
accumulateInSet =
  foldl'
    ( \acc x ->
        if x == '\n'
          then acc
          else S.insert x acc
    )
    emptySet

exampleData :: IO Text
exampleData = pure "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb"