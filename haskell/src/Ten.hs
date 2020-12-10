{-# LANGUAGE OverloadedStrings #-}

module Ten where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Data.Attoparsec.Text (Parser, anyChar, char, decimal, isHorizontalSpace, many1', manyTill', parseOnly, satisfy, string)
import qualified Data.IntMap.Strict as IntMap
import Data.List (find, permutations, sort, span, splitAt, subsequences, (!!))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import MyLib (countTrue)

tenPartOne :: IO ()
tenPartOne = do
  contents <- lines <$> readFile "../data/10.txt"
  example1 <- lines <$> readFile "./example-data/10_1.txt"
  let ints = fmap read contents :: [Int]
  let r = findJoltageDifference (sort ints)
  case r of
    Nothing -> error "Error"
    Just (o, t) -> do
      print $ o * (t + 1)
  return ()

tenPartTwo :: IO ()
tenPartTwo = do
  contents <- lines <$> readFile "../data/10.txt"
  example1 <- lines <$> readFile "./example-data/10_1.txt"
  let list = sort $ fmap read contents :: [Int]
  let ints = 0 : list ++ [maximum list + 3]
  print ints
  let fp = IntMap.lookup (maximum ints) (populateMap ints initialMap)
  print fp
  return ()

findJoltageDifference :: [Int] -> Maybe (Int, Int)
findJoltageDifference ints =
  scanJoltList 0 ints (0, 0)

scanJoltList :: Int -> [Int] -> (Int, Int) -> Maybe (Int, Int)
scanJoltList _ [] c = Just c
scanJoltList n (x : rest) (ones, threes)
  | x - n == 1 = scanJoltList x rest (ones + 1, threes)
  | x - n == 2 = scanJoltList x rest (ones, threes)
  | x - n == 3 = scanJoltList x rest (ones, threes + 1)
  | otherwise = Nothing

initialMap :: IntMap.IntMap Int
initialMap = IntMap.fromList [(0, 1)]

populateMap :: [Int] -> IntMap.IntMap Int -> IntMap.IntMap Int
populateMap (x : xs) map = populateMap xs populatedMap
  where
    successors = possibleSuccessors (x : xs)
    numPaths = fromMaybe 0 (IntMap.lookup x map)
    populatedMap = foldl (\newMap key -> IntMap.insertWith (+) key numPaths newMap) map successors
populateMap _ map = map

possibleSuccessors :: [Int] -> [Int]
possibleSuccessors (x : xs) = takeWhile (\i -> i <= x + 3) xs