{-# LANGUAGE OverloadedStrings #-}

module Day11 where

import Control.Monad (guard)
import Data.Sequence (Seq, fromList)
import qualified Data.Sequence as Seq
import MyLib (count)

data Seats = Seats
  { seq :: Seq (Seq Char),
    width :: Int,
    height :: Int
  }
  deriving (Show, Eq)

ePartOne :: IO ()
ePartOne = do
  contents <- lines <$> readFile "../data/11.txt"
  example1 <- lines <$> readFile "./example-data/11.txt"
  let seq = Seq.fromList (Seq.fromList <$> contents)
      width = Seq.length (Seq.index seq 0)
      height = Seq.length seq
  print seq
  print width
  print height
  let result = loop $ Seats seq width height
  print result
  return ()

elevenPartTwo :: IO ()
elevenPartTwo = do
  contents <- lines <$> readFile "../data/11.txt"
  example1 <- lines <$> readFile "./example-data/11.txt"
  let seq = Seq.fromList (Seq.fromList <$> contents)
      width = Seq.length (Seq.index seq 0)
      height = Seq.length seq
  print seq
  print width
  print height
  let result = loop2 $ Seats seq width height
  print result
  return ()

loop :: Seats -> Int
loop seats =
  let newSeats@(Seats seq _ _) = applySeatingRules seats
   in if seats == newSeats
        then sum (Seq.length . Seq.filter (== '#') <$> seq)
        else loop newSeats

applySeatingRules :: Seats -> Seats
applySeatingRules s@(Seats seq width height) =
  Seats
    ( Seq.fromList $ do
        i <- [0 .. height - 1]
        return $
          Seq.fromList $ do
            j <- [0 .. width - 1]
            let current = Seq.index (Seq.index seq i) j
                neighbs = getNeighbours (i, j) s
            return $ getNewSeatState current neighbs
    )
    width
    height

getNeighbours :: (Int, Int) -> Seats -> [Char]
getNeighbours (i, j) (Seats seq width height) = init $ do
  i' <- [i - 1, i + 1, i]
  j' <- [j - 1, j + 1, j]
  guard $ i' >= 0 && i' < height
  guard $ j' >= 0 && j' < width
  return $ Seq.index (Seq.index seq i') j'

getElem :: Eq a => Seq a -> Int -> Int -> a
getElem seq i h = Seq.index seq (i `mod` h)

getNewSeatState :: Char -> [Char] -> Char
getNewSeatState '#' ns
  | count '#' ns >= 4 = 'L'
getNewSeatState 'L' ns
  | count '#' ns == 0 = '#'
getNewSeatState c _ = c

----- PART 2 ----

loop2 :: Seats -> Int
loop2 seats =
  let newSeats@(Seats seq _ _) = applyVisibleSeatingRules seats
   in if seats == newSeats
        then sum (Seq.length . Seq.filter (== '#') <$> seq)
        else -- then newSeats
          loop2 newSeats

applyVisibleSeatingRules :: Seats -> Seats
applyVisibleSeatingRules s@(Seats seq width height) =
  Seats
    ( Seq.fromList $ do
        i <- [0 .. height - 1]
        return $
          Seq.fromList $ do
            j <- [0 .. width - 1]
            let current = Seq.index (Seq.index seq i) j
                neighbs = getVisibleElements (i, j) s
            return $ getNewVisibleSeatState current neighbs
    )
    width
    height

getNewVisibleSeatState :: Char -> [Char] -> Char
getNewVisibleSeatState '#' ns
  | count '#' ns >= 5 = 'L'
getNewVisibleSeatState 'L' ns
  | count '#' ns == 0 = '#'
getNewVisibleSeatState c _ = c

type BinaryFunc = Int -> Int -> Int

newtype Op
  = Op BinaryFunc

getVisibleElements :: (Int, Int) -> Seats -> [Char]
getVisibleElements coords seats =
  getTopLeftDiagonalElements coords seats
    <> getTopElements coords seats
    <> getTopRightDiagonalElements coords seats
    <> getRightElements coords seats
    <> getBottomRightDiagonalElements coords seats
    <> getBottomElements coords seats
    <> getBottomLeftDiagonalElements coords seats
    <> getLeftElements coords seats
  where
    getTopLeftDiagonalElements = getDirectionalNeighbs (Op (-)) (Op (-))
    getTopRightDiagonalElements = getDirectionalNeighbs (Op (-)) (Op (+))
    getBottomRightDiagonalElements = getDirectionalNeighbs (Op (+)) (Op (+))
    getBottomLeftDiagonalElements = getDirectionalNeighbs (Op (+)) (Op (-))
    getTopElements = getDirectionalNeighbs (Op (-)) (Op const)
    getBottomElements = getDirectionalNeighbs (Op (+)) (Op const)
    getRightElements = getDirectionalNeighbs (Op const) (Op (+))
    getLeftElements = getDirectionalNeighbs (Op const) (Op (-))

getDirectionalNeighbs :: Op -> Op -> (Int, Int) -> Seats -> [Char]
getDirectionalNeighbs (Op fi) (Op fj) (i, j) (Seats seq width height) = take 1 $ do
  n <- [1 .. width]
  let i' = fi i n
      j' = fj j n
  guard $ i' >= 0 && i' < height
  guard $ j' >= 0 && j' < width
  let c = Seq.index (Seq.index seq i') j'
  guard $ c /= '.'
  return c

s = Seats (fromList [fromList "L.LL.LL.LL", fromList "LLLLLLL.LL", fromList "L.L.L..L..", fromList "LLLL.LL.LL", fromList "L.LL.LL.LL", fromList "L.LLLLL.LL", fromList "..L.L.....", fromList "LLLLLLLLLL", fromList "L.LLLLLL.L", fromList "L.LLLLL.LL"]) 10 10