{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import Data.Bifunctor (second)
import Data.List
  ( foldr1,
    intersect,
    isPrefixOf,
    length,
    nub,
    partition,
    sortBy,
    transpose,
    (\\),
  )
import Data.List.Split (splitOn)
import Data.Ord (comparing)

type Rule = (String, [(Int, Int)])

-- parsers --

intList :: [String] -> [[Int]]
intList = map (map read . splitOn ",")

ignoreTill :: String -> [String] -> [String]
ignoreTill s = drop 1 . dropWhile (not . isPrefixOf s)

parseTicket :: [String] -> [Int]
parseTicket = head . intList . take 1 . ignoreTill "your"

parseNearby :: [String] -> [[Int]]
parseNearby = intList . ignoreTill "nearby"

parseRule :: String -> Rule
parseRule s = (lbl, [(a, b), (c, d)])
  where
    (lbl : rng : _) = splitOn ":" s
    (a : b : c : d : _) = map stoi . splitOrDash $ rng
    splitOrDash = concatMap (splitOn "-") . splitOn "or"
    stoi s = read s :: Int

parseRules :: [String] -> [Rule]
parseRules = map parseRule . filter (/= "") . takeWhile (not . isPrefixOf "your")

-- solutions --

isOfLength :: Int -> [a] -> Bool
isOfLength n l = n == length l

inRange :: Ord a => a -> (a, a) -> Bool
inRange n (s, e) = n >= s && n <= e

removeDupLists :: Eq a => [[a]] -> [a]
removeDupLists = foldr1 intersect . filter (not . isOfLength 0)

validRule :: Ord a => a -> [(a, a)] -> Bool
validRule n = any (inRange n)

possibleFields :: [Rule] -> [Int] -> [String]
possibleFields rx = removeDupLists . map matchField . nub
  where
    matchField n = [fst r | r <- rx, validRule n (snd r)]

fit :: Eq a => [(Int, [a])] -> [(Int, [a])]
fit xs
  | all (isOfLength 1 . snd) xs = xs
  | otherwise = fit (singles ++ rest)
  where
    (singles, unstables) = partition (isOfLength 1 . snd) xs
    stables = concatMap snd singles
    rest = map (second (\\ stables)) unstables

fields :: [Rule] -> [[Int]] -> [String]
fields r = names . fit . fieldsPerPos
  where
    fieldsPerPos = zip [0 ..] . map (possibleFields r) . transpose
    names = concatMap snd . sortBy (comparing fst)

extract :: [Int] -> [String] -> [Int]
extract yt = map fst . filter (isPrefixOf "departure" . snd) . zip yt

part2 :: [Rule] -> [[Int]] -> [Int] -> Int
part2 r nb yt = product . extract yt . fields r $ nb

part1 :: [Rule] -> [[Int]] -> Int
part1 r = sum . concatMap invalidFields
  where
    invalidFields tx = [n | n <- tx, not . any (validRule n . snd) $ r]

ticket :: [String] -> (Int, Int)
ticket xs = (part1 r nb, part2 r nb t)
  where
    nb = parseNearby xs
    t = parseTicket xs
    r = parseRules xs

sixteen :: IO ()
sixteen = do
  inp <- lines <$> readFile "../data/16.txt"
  print $ ticket inp