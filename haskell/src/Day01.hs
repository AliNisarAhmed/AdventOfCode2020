module Day01 where

import Data.List (sort)

one :: IO ()
one = do
  contents <- readFile "../data/1.txt"
  let ints = sort $ parseData contents
  print $ sumTo2020 ints (reverse ints)
  return ()

two :: IO ()
two = do
  contents <- readFile "../data/1.txt"
  let ints = parseData contents
  print $ sumThreeTo2020 ints
  return ()

parseData :: String -> [Int]
parseData = map read . lines

sumTo2020 :: [Int] -> [Int] -> Maybe Int
sumTo2020 [] _ = Nothing
sumTo2020 _ [] = Nothing
sumTo2020 first@(x : xs) second@(y : ys)
  | x + y == 2020 = Just $ x * y
  | x + y < 2020 = sumTo2020 xs second
  | otherwise = sumTo2020 first ys

sumThreeTo2020 :: [Int] -> Int
sumThreeTo2020 xs = head $ [a * b * c | a <- xs, b <- xs, c <- xs, a + b + c == 2020]