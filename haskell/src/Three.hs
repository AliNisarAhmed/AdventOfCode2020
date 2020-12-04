{-# LANGUAGE OverloadedStrings #-}

module Three where

import Data.Vector ((!), (!?))
import qualified Data.Vector as V

type Width = Int

data Forest = Forest
  { forest :: V.Vector (V.Vector Char),
    width :: Width
  }
  deriving (Show)

type DownIncr = Int

type RightIncr = Int

type Increment = (DownIncr, RightIncr)

type Row = Int

type Col = Int

type Pos = (Col, Row)

---- Solutions ----

threePartOne :: IO ()
threePartOne = do
  contents <- lines <$> readFile "../data/3.txt"
  let forest = parseData contents
      result = countTrees forest (1, 3) (0, 0) 0
  print result
  return ()

threePartTwo :: IO ()
threePartTwo = do
  contents <- lines <$> readFile "../data/3.txt"
  let forest = parseData contents
      result1 = countTrees forest (1, 1) (0, 0) 0
      result2 = countTrees forest (1, 3) (0, 0) 0
      result3 = countTrees forest (1, 5) (0, 0) 0
      result4 = countTrees forest (1, 7) (0, 0) 0
      result5 = countTrees forest (2, 1) (0, 0) 0
  print result1
  print result2
  print result3
  print result4
  print result5
  print $ result1 * result2 * result3 * result4 * result5
  return ()

parseData :: [String] -> Forest
parseData contents =
  let forest = V.fromList $ fmap V.fromList contents
      width = V.length $ V.head forest
   in Forest {forest = forest, width = width}

countTrees :: Forest -> Increment -> Pos -> Int -> Int
countTrees f@(Forest forest width) (colInc, rowInc) (col, row) count =
  let mr = (!?) forest (col + colInc)
   in case mr of
        Nothing -> count
        Just r ->
          let c = (!) r nr
           in if isTree c
                then countTrees f (colInc, rowInc) (col + colInc, nr) (count + 1)
                else countTrees f (colInc, rowInc) (col + colInc, nr) count
  where
    nr = newRow row rowInc width

newRow :: Row -> RightIncr -> Int -> Int
newRow prevRow rowIncr width =
  rem (prevRow + rowIncr) width

isTree :: Char -> Bool
isTree = (== '#')