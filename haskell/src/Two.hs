{-# LANGUAGE OverloadedStrings #-}

module Two where

import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Input = Input
  { range :: (Int, Int),
    targetLetter :: Char,
    sentence :: T.Text
  }
  deriving (Show)

partOne :: IO ()
partOne = do
  contents <- TIO.readFile "../data/2.txt"
  let input = parseData contents
  case input of
    Left e -> do
      print e
      return ()
    Right listOfInput -> do
      let result = length $ filter checkCountWithinRange listOfInput
      print result
      return ()

partTwo :: IO ()
partTwo = do
  contents <- TIO.readFile "../data/2.txt"
  let input = parseData contents
  case input of
    Left e -> do
      print e
      return ()
    Right listOfInput -> do
      let result = length $ filter checkCharAtPosition listOfInput
      print result
      return ()

---- Main Logic ----

checkCountWithinRange :: Input -> Bool
checkCountWithinRange (Input (l, u) c s) =
  count >= l && count <= u
  where
    count = countChar c s

countChar :: Char -> T.Text -> Int
countChar c = T.length . T.filter (c ==)

checkCharAtPosition :: Input -> Bool
checkCharAtPosition (Input (p1, p2) c s) =
  not (b1 && b2) && (b1 || b2)
  where
    string = T.unpack s
    b1 = string !! (p1 - 1) == c
    b2 = string !! (p2 - 1) == c

---- Parsing ----

parseData :: T.Text -> Either String [Input]
parseData t = sequenceA $ parseOnly parseLine <$> T.lines t

parseLine :: Parser Input
parseLine = do
  r <- parseRange
  space
  c <- parseTargetLetter
  space
  s <- parseSentence
  return $ Input r c s

parseRange :: Parser (Int, Int)
parseRange = do
  n1 <- decimal
  char '-'
  n2 <- decimal
  return (n1, n2)

parseTargetLetter :: Parser Char
parseTargetLetter = do
  c <- letter
  char ':'
  return c

parseSentence :: Parser T.Text
parseSentence = takeText