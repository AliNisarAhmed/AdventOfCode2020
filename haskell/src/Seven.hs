{-# LANGUAGE OverloadedStrings #-}

module Seven where

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.Text (Parser, anyChar, char, decimal, isHorizontalSpace, many1', manyTill', parseOnly, satisfy, string)
import Data.Graph (graphFromEdges, path, vertices)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import MyLib (countTrue)

sevenPartOne :: IO ()
sevenPartOne = do
  contents <- T.lines <$> TIO.readFile "../data/7.txt"
  e <- T.lines <$> TIO.readFile "./example-data/7-1.txt"
  let mr = traverse (parseOnly parseSentence) contents
  case mr of
    Left e -> print e
    Right d -> do
      let (g, vf, kf) = graphFromEdges d
          vs = vertices g
          targetvertex = kf $ T.unpack target
      case targetvertex of
        Nothing -> return ()
        Just tv -> do
          let c1 = map (\v -> path g v tv) (filter (/= tv) vs)
          print $ countTrue c1
          return ()
      return ()

sevenPartTwo :: IO ()
sevenPartTwo = do
  contents <- T.lines <$> TIO.readFile "../data/7.txt"
  let e = T.lines <$> TIO.readFile "./example-data/7-2.txt"
  let mr = traverse (parseOnly parseSentence2) contents
  case mr of
    Left e -> print e
    Right d -> do
      print $ countBags d target - 1
      return ()
  return ()

----- Parsers for Part One ----

spaceOrNewLine :: Parser Char
spaceOrNewLine = do
  char '\n' <|> satisfy isHorizontalSpace

parseSentence :: Parser (String, String, [String])
parseSentence = do
  s1 <- manyTill' anyChar spaceOrNewLine
  s2 <- manyTill' anyChar spaceOrNewLine
  string "bags contain "
  bags <- many1' bagParser <|> noOtherBags
  return (s1 <> " " <> s2, s1 <> " " <> s2, bags)

noOtherBags :: Parser [String]
noOtherBags = do
  string "no other bags."
  return []

bagParser :: Parser String
bagParser = do
  n <- decimal
  spaceOrNewLine
  s1 <- manyTill' anyChar spaceOrNewLine
  s2 <- manyTill' anyChar spaceOrNewLine
  string "bags" <|> string "bag"
  string ", " <|> string "."
  return $ s1 <> " " <> s2

-------------------------------------------
--- Parser for Part 2 ---

bagAndCountParser :: Parser (Int, Text)
bagAndCountParser = do
  n <- decimal
  spaceOrNewLine
  s1 <- manyTill' anyChar spaceOrNewLine
  s2 <- manyTill' anyChar spaceOrNewLine
  string "bags" <|> string "bag"
  string ", " <|> string "."
  return (n, T.pack $ s1 <> " " <> s2)

noOtherBags2 :: Parser [(Int, Text)]
noOtherBags2 = do
  string "no other bags."
  return []

parseSentence2 :: Parser (Text, [(Int, Text)])
parseSentence2 = do
  s1 <- manyTill' anyChar spaceOrNewLine
  s2 <- manyTill' anyChar spaceOrNewLine
  string "bags contain "
  bags <- many1' bagAndCountParser <|> noOtherBags2
  let bagName = T.pack $ s1 <> " " <> s2
  return (bagName, bags)

target :: Text
target = "shiny gold"

countBags :: [(Text, [(Int, Text)])] -> Text -> Int
countBags g target =
  case find ((== target) . fst) g of
    Nothing -> 0
    Just (_, subList) ->
      let subWeights = sum $ map (\(w, s) -> w * countBags g s) subList
       in 1 + subWeights
