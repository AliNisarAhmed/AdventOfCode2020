{-# LANGUAGE OverloadedStrings #-}

---- NOT The best solution - need to learn how to parse expressions into an AST with proper precedence of operations
------ search shunting yard algorithm

module Day18 where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard, void)
import Data.Attoparsec.Text (IResult (Done, Fail, Partial), Parser, anyChar, char, decimal, endOfInput, endOfLine, feed, isHorizontalSpace, many1', manyTill', parse, parseOnly, parseTest, peekChar', satisfy, space, string)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.IntMap.Strict as IntMap
import Data.List (find, nub, partition, permutations, sort, span, splitAt, subsequences, union, (!!))
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Debug.Trace (trace)
import MyLib (countTrue)

etwo :: IO ()
etwo = do
  contents <- T.lines <$> TIO.readFile "../data/18.txt"
  example <- T.lines <$> TIO.readFile "./example-data/18.txt"
  let ex = traverse (parseOnly evalExpr2) $ fmap T.reverse contents
  case ex of
    Left e -> print e
    Right l -> print $ sum l
  return ()

evalExpr2 :: Parser Int
evalExpr2 = do
  evalMul2 <|> evalTerm

evalTerm :: Parser Int
evalTerm = do
  evalAdd2 <|> evalFactor2

evalFactor2 :: Parser Int
evalFactor2 =
  decimal <|> do
    char ')'
    e <- evalExpr2
    char '('
    return e

evalMul2 :: Parser Int
evalMul2 = do
  n1 <- evalTerm
  space
  char '*'
  space
  e <- evalExpr2
  return $ n1 * e

evalAdd2 :: Parser Int
evalAdd2 = do
  n1 <- evalFactor2
  space
  char '+'
  space
  n2 <- evalFactor2
  let s = n1 + n2
  parseOp s

parseOp :: Int -> Parser Int
parseOp n =
  ( do
      space
      char '+'
      space
      f <- evalFactor2
      parseOp (n + f)
  )
    <|> ( do
            space
            char '*'
            space
            (n *) <$> evalExpr2
        )
    <|> return n

e1 :: Text
e1 = "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"

-------------------

eone :: IO ()
eone = do
  contents <- T.lines <$> TIO.readFile "../data/18.txt"
  example <- T.lines <$> TIO.readFile "./example-data/18.txt"
  let ex = traverse (parseOnly evalExpr) $ fmap T.reverse contents
  case ex of
    Left e -> print e
    Right l -> print $ sum l
  return ()

evalExpr :: Parser Int
evalExpr = do
  evalMul <|> evalAdd <|> evalFactor

evalFactor :: Parser Int
evalFactor =
  decimal <|> do
    char ')'
    e <- evalExpr
    char '('
    return e

evalMul :: Parser Int
evalMul = do
  n1 <- evalFactor
  space
  char '*'
  space
  e <- evalExpr
  return $ n1 * e

evalAdd :: Parser Int
evalAdd = do
  n1 <- evalFactor
  space
  char '+'
  space
  e <- evalExpr
  return $ n1 + e
