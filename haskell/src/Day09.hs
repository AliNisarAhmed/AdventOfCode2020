{-# LANGUAGE OverloadedStrings #-}

module Day09 where

import Control.Monad (guard)
import Data.List (sort)
import Data.List.Split (divvy)

ninePartOne :: IO ()
ninePartOne = do
  contents <- lines <$> readFile "../data/9.txt"
  let ints = fmap read contents :: [Integer]
      result = findFirstInvalid ints
  print result
  return ()

ninePartTwo :: IO ()
ninePartTwo = do
  contents <- lines <$> readFile "../data/9.txt"
  let ints = fmap read contents :: [Integer]
      result = findContigSum ints
  print result
  return ()

findFirstInvalid :: [Integer] -> Integer
findFirstInvalid ints =
  let (first25, xs@(current : _)) = splitAt 25 ints
      sorted = sort first25
   in if checkIfValid sorted (reverse sorted) current
        then findFirstInvalid (drop 1 first25 ++ xs)
        else current

checkIfValid :: [Integer] -> [Integer] -> Integer -> Bool
checkIfValid [] _ _ = False
checkIfValid _ [] _ = False
checkIfValid first@(x : xs) second@(y : ys) target
  | x + y == target = True
  | x + y < target = checkIfValid xs second target
  | otherwise = checkIfValid first ys target

partTwoTarget :: Integer
partTwoTarget = 85848519

findContigSum :: [Integer] -> [Integer]
findContigSum ints = do
  cat <- [2 .. length ints]
  list <- divvy cat 1 ints
  guard $ sum list == partTwoTarget
  return $ maximum list + minimum list