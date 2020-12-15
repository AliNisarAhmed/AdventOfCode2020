{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq, ViewR ((:>)), (|>))
import qualified Data.Sequence as Seq

fifteen :: IO ()
fifteen = do
  example1 <- readFile "./example-data/15.txt"
  let parsed = readInt <$> splitOn "," example1
  let input = Seq.fromList parsed
  let m = M.fromList $ zipWith (\i e -> (e, [i])) [1 ..] parsed
  let result = iterateSeq2 (Seq.length input + 1) m input
  let _ :> last = Seq.viewr result
  print last
  return ()

iterateSeq2 :: Int -> Map Int [Int] -> Seq Int -> Seq Int
iterateSeq2 30000001 _ seq = seq
iterateSeq2 turn m seq =
  case M.lookup lastElem m of
    Nothing ->
      case nextStep seq of
        Nothing -> iterateSeq2 (turn + 1) (updateMap 0 turn m) (seq |> 0)
        Just (i1, i2) -> iterateSeq2 (turn + 1) (updateMap (i1 - i2) turn m) (seq |> (i1 - i2))
    Just (i1 : i2 : _) -> iterateSeq2 (turn + 1) (updateMap (i1 - i2) turn m) (seq |> i1 - i2)
    Just _ -> iterateSeq2 (turn + 1) (updateMap 0 turn m) (seq |> 0)
  where
    _ :> lastElem = Seq.viewr seq
    updateMap k t = M.insertWith (++) k [t]

nextStep :: Seq Int -> Maybe (Int, Int)
nextStep seq =
  case Seq.elemIndicesR lastElem seq of
    (i1 : i2 : _) -> Just (i1 + 1, i2 + 1)
    _ -> Nothing
  where
    _ :> lastElem = Seq.viewr seq

readInt :: String -> Int
readInt = read