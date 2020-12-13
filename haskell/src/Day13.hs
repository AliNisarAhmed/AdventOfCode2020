{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import Control.Monad (guard)
import Data.Foldable (Foldable (foldl'))
import Data.List (minimumBy)
import Data.List.Split (splitOn)

thpartone :: IO ()
thpartone = do
  contents <- lines <$> readFile "../data/13.txt"
  example1 <- lines <$> readFile "./example-data/13.txt"
  let [t, busIdsWithXs] = contents
      timeStamp = read t :: Int
      (id, [minTime]) = minimumBy (\(_, [t1]) (_, [t2]) -> compare t1 t2) $ map (\id -> (id, multiplesList id timeStamp)) $ fmap readInt $ filter (/= "x") $ splitOn "," busIdsWithXs
  print $ (minTime - timeStamp) * id
  return ()

thPartTwo :: IO ()
thPartTwo = do
  contents <- lines <$> readFile "../data/13.txt"
  example1 <- lines <$> readFile "./example-data/13.txt"
  let [t, busIdsWithXs] = contents
      busIds = fmap (\(i, s) -> (i, read s)) $ filter (\(i, s) -> s /= "x") $ zip [0 ..] $ splitOn "," busIdsWithXs
  let result = part2 busIds
  print result
  return ()

multiplesList :: Int -> Int -> [Int]
multiplesList n t = take 1 $ do
  k <- [1 ..]
  guard $ n * k > t
  pure $ n * k

commmonMultiples :: Int -> (Int, Int) -> [Int]
commmonMultiples x (j, y) = do
  k <- [1 ..]
  guard $ (j + k * x) `mod` y == 0
  return $ k * x

readInt :: String -> Int
readInt = read

part2 :: [(Int, Int)] -> Int
part2 = fst . foldl' go (0, 1)
  where
    go (!base, !step) (offset, i) = (base', step * i)
      where
        base' =
          until
            (\n -> (n + offset) `mod` i == 0)
            (+ step)
            base
