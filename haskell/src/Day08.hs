{-# LANGUAGE OverloadedStrings #-}

module Day08 where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Data.Attoparsec.Text (Parser, anyChar, char, decimal, isHorizontalSpace, many1', manyTill', parseOnly, satisfy, string)
import Data.List (find, splitAt, (!!))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import MyLib (countTrue)

data Expr
  = NoOp Int
  | Acc Int
  | Jump Int
  deriving (Eq, Show)

eightPartOne :: IO ()
eightPartOne = do
  contents <- T.lines <$> TIO.readFile "../data/8.txt"
  let er = traverse (parseOnly parseExpr) contents
  case er of
    Left e -> print e
    Right r -> do
      let lst = zip [0 ..] r
          result = executeExpressions 0 lst (head lst) []
      print result
      return ()
  return ()

ePartTwo :: IO ()
ePartTwo = do
  contents <- T.lines <$> TIO.readFile "../data/8.txt"
  let er = traverse (parseOnly parseExpr) contents
  case er of
    Left e -> print e
    Right r -> do
      let lst = zip [0 ..] r
          result = executeExpressions2 0 lst (head lst) []
      print result
      return ()
  return ()

executeExpressions :: Int -> [(Int, Expr)] -> (Int, Expr) -> [Int] -> Int
executeExpressions acc list (index, currentExpr) alreadyExecuted =
  if index `elem` alreadyExecuted
    then acc
    else case currentExpr of
      NoOp _ -> executeExpressions acc list (list !! (index + 1)) (index : alreadyExecuted)
      Acc n -> executeExpressions (acc + n) list (list !! (index + 1)) (index : alreadyExecuted)
      Jump n -> executeExpressions acc list (list !! (index + n)) (index : alreadyExecuted)

executeExpressions2 :: Int -> [(Int, Expr)] -> (Int, Expr) -> [Int] -> Int
executeExpressions2 acc list (index, currentExpr) alreadyExecuted =
  case currentExpr of
    Acc n ->
      if index + 1 == length list
        then acc
        else executeExpressions2 (acc + n) list (list !! (index + 1)) (index : alreadyExecuted)
    NoOp n ->
      let newExpr = (index, Jump n)
          newList = insertInList newExpr list
          mAcc = checkIfTerminates acc newList newExpr alreadyExecuted
       in case mAcc of
            Nothing -> executeExpressions2 acc list (list !! (index + 1)) (index : alreadyExecuted)
            Just c -> c
    Jump n ->
      let newExpr = (index, NoOp n)
          newList = insertInList newExpr list
          mAcc = checkIfTerminates acc newList newExpr alreadyExecuted
       in case mAcc of
            Nothing -> executeExpressions2 acc list (list !! (index + n)) (index : alreadyExecuted)
            Just c -> c

checkIfTerminates :: Int -> [(Int, Expr)] -> (Int, Expr) -> [Int] -> Maybe Int
checkIfTerminates acc list (index, currentExpr) alreadyExecuted
  | index `elem` alreadyExecuted = Nothing
  | index + 1 == length list = Just acc
  | otherwise = case currentExpr of
    NoOp _ -> checkIfTerminates acc list (list !! (index + 1)) (index : alreadyExecuted)
    Acc n -> checkIfTerminates (acc + n) list (list !! (index + 1)) (index : alreadyExecuted)
    Jump n -> checkIfTerminates acc list (list !! (index + n)) (index : alreadyExecuted)

insertInList :: (Int, Expr) -> [(Int, Expr)] -> [(Int, Expr)]
insertInList (i, e2) list =
  let (first, _ : rest) = splitAt i list
   in first ++ [(i, e2)] ++ rest

--- Parsers --

parseExpr :: Parser Expr
parseExpr = parserNoOp <|> parserAcc <|> parserJmp

parserNoOp :: Parser Expr
parserNoOp = do
  string "nop "
  c <- char '+' <|> char '-'
  n <- decimal
  case c of
    '+' -> return $ NoOp n
    _ -> return $ NoOp (- n)

parserAcc :: Parser Expr
parserAcc = do
  string "acc "
  c <- char '+' <|> char '-'
  n <- decimal
  case c of
    '+' -> return $ Acc n
    _ -> return $ Acc (- n)

parserJmp :: Parser Expr
parserJmp = do
  string "jmp "
  c <- char '+' <|> char '-'
  n <- decimal
  case c of
    '+' -> return $ Jump n
    _ -> return $ Jump (- n)