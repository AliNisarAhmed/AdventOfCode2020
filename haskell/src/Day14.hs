{-# LANGUAGE OverloadedStrings #-}

---- !!!!!!!!!!!!!!!!! LESSON LEARNED !!!!!!!!!!!!!!!!!!!!!!!
---- !!!!!!!!!!!!!!!!! DO NOT TRUST THE ORDER OF ITERATION IN MAPS AND OBJECTS !!!!!!!!!!!!!!!!!!!!

module Day14 where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Data.Attoparsec.Text (Parser, anyChar, char, choice, decimal, isHorizontalSpace, many1', manyTill', parseOnly, satisfy, sepBy, string)
import Data.Char (intToDigit)
import Data.Foldable (Foldable (foldl', toList))
import qualified Data.IntMap.Strict as IntMap
import Data.List (find, permutations, sort, span, splitAt, subsequences, unfoldr, (!!))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (Sum (Sum), getSum)
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Text (Text, splitOn)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import MyLib (countTrue)

fpartone :: IO ()
fpartone = do
  contents <- TIO.readFile "../data/14.txt"
  example1 <- TIO.readFile "./example-data/14-1.txt"
  let d = filter (/= "") <$> splitOn "mask = " $ contents
  -- print d
  let i = traverse parseInput d
  case i of
    Left e -> error "e"
    Right v -> do
      let r1 = applyMask v M.empty
      print r1
      let r2 = sumAllBinary r1
      print r2
  return ()

fPartTwo :: IO ()
fPartTwo = do
  contents <- TIO.readFile "../data/14.txt"
  example1 <- TIO.readFile "./example-data/14-2.txt"
  let d = filter (/= "") <$> splitOn "mask = " $ contents
  let i = traverse parseInput2 d
  case i of
    Left e -> error "e"
    Right v -> do
      let r1 = M.unions $ reverse $ M.fromList . applyMask2 <$> v
      print $ M.foldr' (+) 0 r1
  return ()

data MaskedInstruction = MI
  { mask :: Seq Char,
    instr :: Map Int (Seq Char)
  }
  deriving (Eq, Show)

data MaskedInstruction2 = MI2
  { m :: Seq Char,
    i :: [(Int, Int)]
  }
  deriving (Eq, Show)

defaultMi :: MaskedInstruction
defaultMi = MI Seq.empty M.empty

defaultMi2 :: MaskedInstruction2
defaultMi2 = MI2 Seq.empty []

applyMask2 :: MaskedInstruction2 -> [(Int, Int)]
applyMask2 (MI2 mask m) =
  concatMap (\(k, v) -> zip (maskKey (convertKey k) mask) (repeat v)) m

convertKey :: Int -> Seq Char
convertKey k = Seq.fromList $ lpad 36 $ dec2bin k

maskKey :: Seq Char -> Seq Char -> [Int]
maskKey k mask = toList $ fmap bin2dec $ scramble $ newMaskRules k mask

scramble :: Seq Char -> Seq (Seq Char)
scramble xs =
  let (f, rest) = Seq.breakl (== 'X') xs
   in if Seq.null rest
        then Seq.singleton f
        else scramble (f >< Seq.singleton '1' >< Seq.drop 1 rest) >< scramble (f >< Seq.singleton '0' >< Seq.drop 1 rest)

newMaskRules :: Seq Char -> Seq Char -> Seq Char
newMaskRules = Seq.zipWith zipper
  where
    zipper k m =
      case m of
        '0' -> k
        _ -> m

----- Part One ---

sumAllBinary :: Map Int (Seq Char) -> Int
sumAllBinary = M.foldr step 0
  where
    step :: Seq Char -> Int -> Int
    step s acc = bin2dec s + acc

applyMask :: [MaskedInstruction] -> Map Int (Seq Char) -> Map Int (Seq Char)
applyMask [] acc = acc
applyMask ((MI mask map) : ms) acc =
  applyMask ms (M.union (M.map mapper map) acc)
  where
    mapper :: Seq Char -> Seq Char
    mapper = Seq.zipWith zipper mask
    zipper :: Char -> Char -> Char
    zipper m c =
      case m of
        'X' -> c
        v -> v

parseInput2 :: Text -> Either String MaskedInstruction2
parseInput2 = parseOnly instructionParser2

parseInput :: Text -> Either String MaskedInstruction
parseInput = parseOnly instructionParser

memValueParser2 :: Parser Int
memValueParser2 = do
  decimal

memParser2 :: Parser (Int, Int)
memParser2 = do
  a <- memAddressParser
  v <- memValueParser2
  return (a, v)

instructionParser2 :: Parser MaskedInstruction2
instructionParser2 = do
  m <- maskParser
  is <- memParser2 `sepBy` char '\n'
  return $ MI2 m is

---- Parsers ---

instructionParser :: Parser MaskedInstruction
instructionParser = do
  m <- maskParser
  is <- memParser `sepBy` char '\n'
  return $ MI m (M.unions is)

maskParser :: Parser (Seq Char)
maskParser =
  Seq.fromList <$> do
    cs <- many1' $ char 'X' <|> char '1' <|> char '0'
    char '\n'
    return cs

memParser :: Parser (Map Int (Seq Char))
memParser =
  M.fromList <$> do
    a <- memAddressParser
    v <- memValueParser
    return [(a, Seq.fromList $ lpad 36 v)]

memAddressParser :: Parser Int
memAddressParser = do
  string "mem["
  n <- decimal
  char ']'
  string " = "
  return n

memValueParser :: Parser String
memValueParser = do
  n <- decimal
  return $ dec2bin n

--- Helper Functions ---

lpad :: Int -> [Char] -> [Char]
lpad m xs = replicate (m - length ys) '0' ++ ys
  where
    ys = take m xs

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = toBinary (n `quot` 2) ++ [n `rem` 2]

dec2bin :: Int -> [Char]
dec2bin = reverse . map intToDigit . unfoldr (\x -> if x == 0 then Nothing else Just (rem x 2, div x 2))

bin2dec :: Seq Char -> Int
bin2dec s = getSum $ Seq.foldMapWithIndex mapper $ Seq.reverse s
  where
    mapper :: Int -> Char -> Sum Int
    mapper _ '0' = Sum 0
    mapper i '1' = Sum $ 2 ^ i

-- 4272107924611
-- 4448782814933
-- 4296862318717
-- 4239402876469
-- 4255753714735
-- 4288986482164
