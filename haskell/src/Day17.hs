{-# LANGUAGE OverloadedStrings #-}

---- VERY SLOW for part 2 (because Lists) - better solution here using IntSets: https://github.com/yongrenjie/aoc20-hs/blob/master/d17.hs

module Day17 where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Data.Attoparsec.Text (Parser, anyChar, char, decimal, isHorizontalSpace, many1', manyTill', parseOnly, satisfy, string)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.IntMap.Strict as IntMap
import Data.List (find, nub, partition, permutations, sort, span, splitAt, subsequences, union, (!!))
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import MyLib (countTrue)

data State
  = Active
  | InActive
  deriving (Eq, Show)

data Point4d = Point4d
  { s :: State,
    c :: (Int, Int, Int, Int)
  }
  deriving (Show)

instance Eq Point4d where
  (==) (Point4d _ c1) (Point4d _ c2) = c1 == c2

instance Ord Point4d where
  compare (Point4d _ c1) (Point4d _ c2) = compare c1 c2

sevPartTwo :: IO ()
sevPartTwo = do
  contents <- lines <$> readFile "../data/17.txt"
  example <- lines <$> readFile "./example-data/17.txt"
  print example
  let d = filter isStateActive2 $ make4dPoint 0 0 <$> makeGrid contents
  let r1 = applyRules2 d
  let r2 = applyRules2 r1
  let r3 = applyRules2 r2
  let r4 = applyRules2 r3
  let r5 = applyRules2 r4
  let r6 = applyRules2 r5
  print $ length $ filter isStateActive2 r6
  return ()

applyRules2 :: [Point4d] -> [Point4d]
applyRules2 activeSpace = handleActives2 activeSpace ++ handleInactives2 activeSpace

handleInactives2 :: [Point4d] -> [Point4d]
handleInactives2 activeSpace =
  foldr step [] (generateInactiveNeighbs2 activeSpace)
  where
    step p acc =
      case applyInactiveRules2 activeSpace p of
        ap@(Point4d Active _) -> ap : acc
        _ -> acc

applyInactiveRules2 :: [Point4d] -> Point4d -> Point4d
applyInactiveRules2 activeSpace pt@(Point4d InActive _)
  | ca == 3 = pt {s = Active}
  | otherwise = pt
  where
    ca = countActives2 activeSpace pt

generateInactiveNeighbs2 :: [Point4d] -> [Point4d]
generateInactiveNeighbs2 activeSpace =
  S.toList $ S.fromList $ foldr step [] activeSpace
  where
    step p acc = neighInactivePoints2 activeSpace p ++ acc

neighInactivePoints2 :: [Point4d] -> Point4d -> [Point4d]
neighInactivePoints2 activeSpace (Point4d _ (x, y, z, w)) = init $ do
  o1 <- [x - 1, x + 1, x]
  o2 <- [y - 1, y + 1, y]
  o3 <- [z - 1, z + 1, z]
  o4 <- [w - 1, w + 1, w]
  guard $ notElem (Point4d Active (o1, o2, o3, o4)) activeSpace
  return $ Point4d InActive (o1, o2, o3, o4)

handleActives2 :: [Point4d] -> [Point4d]
handleActives2 activeSpace =
  foldr step [] activeSpace
  where
    step p acc =
      case applyActiveRules2 activeSpace p of
        ap@(Point4d Active _) -> ap : acc
        _ -> acc

applyActiveRules2 :: [Point4d] -> Point4d -> Point4d
applyActiveRules2 activeSpace pt
  | ca == 2 || ca == 3 = pt
  | otherwise = pt {s = InActive}
  where
    ca = countActives2 activeSpace pt

countActives2 :: [Point4d] -> Point4d -> Int
countActives2 activeSpace pt = length $ filter (isNeighb2 pt) activeSpace

isNeighb2 :: Point4d -> Point4d -> Bool
isNeighb2 (Point4d _ (x, y, z, w)) (Point4d _ (a, b, c, d))
  | x == a && y == b && z == c = wd
  | y == b && z == c && w == d = xa
  | z == c && w == d && x == a = yb
  | w == d && x == a && y == b = zc
  | x == a && y == b = zc && wd
  | y == b && z == c = xa && wd
  | z == c && x == a = yb && wd
  | x == a && w == d = yb && zc
  | y == b && w == d = xa && zc
  | z == c && w == d = xa && yb
  | x == a = yb && zc && wd
  | y == b = xa && zc && wd
  | z == c = xa && yb && wd
  | w == d = xa && yb && zc
  | otherwise = xa && yb && zc && wd
  where
    xa = diffByOne x a
    yb = diffByOne y b
    zc = diffByOne z c
    wd = diffByOne w d

isStateActive2 :: Point4d -> Bool
isStateActive2 (Point4d s _) = s == Active

make4dPoint :: Int -> Int -> (Int, Int, Char) -> Point4d
make4dPoint z w (r, c, ch) = Point4d state (r, c, z, w)
  where
    state = case ch of
      '#' -> Active
      _ -> InActive

--------------------- Part One -------------------

data Point = Point
  { state :: State,
    coords :: (Int, Int, Int)
  }
  deriving (Show)

instance Eq Point where
  (==) (Point _ c1) (Point _ c2) = c1 == c2

instance Ord Point where
  compare (Point _ c1) (Point _ c2) = compare c1 c2

sevpartone :: IO ()
sevpartone = do
  contents <- lines <$> readFile "../data/17.txt"
  example <- lines <$> readFile "./example-data/17.txt"
  print example
  let d = filter isStateActive $ makePoint 0 <$> makeGrid contents
  let r1 = applyRules d
  let r2 = applyRules r1
  let r3 = applyRules r2
  let r4 = applyRules r3
  let r5 = applyRules r4
  let r6 = applyRules r5
  let onlyActive = filter isStateActive r6
  print $ length onlyActive
  return ()

applyRules :: [Point] -> [Point]
applyRules activeSpace = handleActives activeSpace ++ handleInactives activeSpace

countNs :: [Point] -> Point -> (Int, Int)
countNs space pt = bimap length length parted
  where
    ns = neighbs space pt
    parted = partition isStateActive ns

isActive :: [Point] -> Point -> Bool
isActive space pt@(Point s _) =
  (pt `elem` space) && (s == Active)

neighbs :: [Point] -> Point -> [Point]
neighbs space (Point s (x, y, z)) = findPointInSpace space <$> ncs
  where
    ncs = neighbCoords (x, y, z)

neighbCoords :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbCoords (x, y, z) = init $ do
  o1 <- [x - 1, x + 1, x]
  o2 <- [y - 1, y + 1, y]
  o3 <- [z - 1, z + 1, z]
  return (o1, o2, o3)

handleInactives :: [Point] -> [Point]
handleInactives activeSpace =
  foldr step [] (generateInactiveNeighbs activeSpace)
  where
    step p acc =
      case applyInactiveRules activeSpace p of
        ap@(Point Active _) -> ap : acc
        _ -> acc

applyInactiveRules :: [Point] -> Point -> Point
applyInactiveRules activeSpace pt@(Point InActive _)
  | ca == 3 = pt {state = Active}
  | otherwise = pt
  where
    ca = countActives activeSpace pt

neighInactivePoints :: [Point] -> Point -> [Point]
neighInactivePoints activeSpace (Point _ (x, y, z)) = init $ do
  o1 <- [x - 1, x + 1, x]
  o2 <- [y - 1, y + 1, y]
  o3 <- [z - 1, z + 1, z]
  guard $ notElem (Point Active (o1, o2, o3)) activeSpace
  return $ Point InActive (o1, o2, o3)

generateInactiveNeighbs :: [Point] -> [Point]
generateInactiveNeighbs activeSpace =
  S.toList $ S.fromList $ foldr step [] activeSpace
  where
    step p acc = neighInactivePoints activeSpace p ++ acc

isStateActive :: Point -> Bool
isStateActive (Point s _) = s == Active

handleActives :: [Point] -> [Point]
handleActives activeSpace =
  foldr step [] activeSpace
  where
    step p acc =
      case applyActiveRules activeSpace p of
        ap@(Point Active _) -> ap : acc
        _ -> acc

applyActiveRules :: [Point] -> Point -> Point
applyActiveRules activeSpace pt
  | ca == 2 || ca == 3 = pt
  | otherwise = pt {state = InActive}
  where
    ca = countActives activeSpace pt

countActives :: [Point] -> Point -> Int
countActives activeSpace pt = length $ filter (isNeighb pt) activeSpace

isNeighb :: Point -> Point -> Bool
isNeighb (Point _ (x, y, z)) (Point _ (a, b, c))
  | x == a && y == b = zc
  | y == b && z == c = xa
  | z == c && x == a = yb
  | x == a = yb && zc
  | y == b = xa && zc
  | z == c = xa && yb
  | otherwise = xa && yb && zc
  where
    xa = diffByOne x a
    yb = diffByOne y b
    zc = diffByOne z c

diffByOne :: Int -> Int -> Bool
diffByOne x y = abs (x - y) == 1

findPointInSpace :: [Point] -> (Int, Int, Int) -> Point
findPointInSpace space c@(x, y, z) =
  case find (\(Point _ v) -> c == v) space of
    Just p -> p
    Nothing -> Point InActive c

makePoint :: Int -> (Int, Int, Char) -> Point
makePoint z (r, c, ch) = Point state (r, c, z)
  where
    state = case ch of
      '#' -> Active
      _ -> InActive

makeGrid :: [String] -> [(Int, Int, Char)]
makeGrid xs = zip3 rows (cycle [1 .. colWidth]) (concat xs)
  where
    rowWidth = length $ head xs
    colWidth = length xs
    rs = [1 .. rowWidth]
    rows = concatMap (replicate rowWidth) rs

px :: Int -> (Point -> Bool)
px n (Point _ (x, _, _)) = x == n

py :: Int -> (Point -> Bool)
py n (Point _ (_, y, _)) = y == n

pz :: Int -> (Point -> Bool)
pz n (Point _ (_, _, z)) = z == n

zn1 :: [String]
zn1 = replicate n (replicate n '.')
  where
    n = 16

d = filter isStateActive $ makePoint 0 <$> makeGrid example

example = [".#..", "..#.", "###.", "...."]

-- space :: [Point]
-- space = [Point {state = InActive, coords = (1, 1, -1)}, Point {state = InActive, coords = (1, 2, -1)}, Point {state = InActive, coords = (1, 3, -1)}, Point {state = InActive, coords = (1, 1, -1)}, Point {state = InActive, coords = (1, 2, -1)}, Point {state = InActive, coords = (2, 3, -1)}, Point {state = InActive, coords = (2, 1, -1)}, Point {state = InActive, coords = (2, 2, -1)}, Point {state = InActive, coords = (2, 3, -1)}, Point {state = InActive, coords = (2, 1, -1)}, Point {state = InActive, coords = (3, 2, -1)}, Point {state = InActive, coords = (3, 3, -1)}, Point {state = InActive, coords = (3, 1, -1)}, Point {state = InActive, coords = (3, 2, -1)}, Point {state = InActive, coords = (3, 3, -1)}, Point {state = InActive, coords = (1, 1, 0)}, Point {state = Active, coords = (1, 2, 0)}, Point {state = InActive, coords = (1, 3, 0)}, Point {state = InActive, coords = (2, 1, 0)}, Point {state = InActive, coords = (2, 2, 0)}, Point {state = Active, coords = (2, 3, 0)}, Point {state = Active, coords = (3, 1, 0)}, Point {state = Active, coords = (3, 2, 0)}, Point {state = Active, coords = (3, 3, 0)}, Point {state = InActive, coords = (1, 1, 1)}, Point {state = InActive, coords = (1, 2, 1)}, Point {state = InActive, coords = (1, 3, 1)}, Point {state = InActive, coords = (1, 1, 1)}, Point {state = InActive, coords = (1, 2, 1)}, Point {state = InActive, coords = (2, 3, 1)}, Point {state = InActive, coords = (2, 1, 1)}, Point {state = InActive, coords = (2, 2, 1)}, Point {state = InActive, coords = (2, 3, 1)}, Point {state = InActive, coords = (2, 1, 1)}, Point {state = InActive, coords = (3, 2, 1)}, Point {state = InActive, coords = (3, 3, 1)}, Point {state = InActive, coords = (3, 1, 1)}, Point {state = InActive, coords = (3, 2, 1)}, Point {state = InActive, coords = (3, 3, 1)}]