{-# LANGUAGE OverloadedStrings #-}

module AOCStarterTemplate where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Data.Attoparsec.Text (Parser, anyChar, char, decimal, isHorizontalSpace, many1', manyTill', parseOnly, satisfy, string)
import Data.List (find, splitAt, (!!))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import MyLib (countTrue)

readData :: FilePath -> IO ()
readData path = do
  contents <- TIO.readFile path
  print contents
  realInput <- parseInput contents
  exampleInput <- parseInput example
  return ()

type Input = [Int]

parseInput :: Text -> IO Input
parseInput t = undefined

example :: Text
example = undefined