{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day04 where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Attoparsec.Text (Parser, anyChar, char, choice, decimal, endOfInput, endOfLine, hexadecimal, isHorizontalSpace, many', many1', manyTill, manyTill', notChar, option, satisfy, sepBy, skip, skipSpace, skipWhile, string)
import qualified Data.Attoparsec.Text as AT
import Data.Foldable (Foldable (foldl'))
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

fourPartOne :: IO ()
fourPartOne = do
  contents <- TIO.readFile "../data/4.txt"
  let result = AT.parseOnly parseAllPassports contents
  case result of
    Left e -> print e
    Right list -> do
      let count = length $ filter id $ fmap (validPassport . makePassport) list
      print count
  return ()

fourPartTwo :: IO ()
fourPartTwo = do
  contents <- TIO.readFile "../data/4.txt"
  let result = AT.parseOnly parseAllPassports contents
  case result of
    Left e -> print e
    Right list -> do
      let count = length $ filter id $ fmap (validPassport . makeAndValidatePassport) list
      print count
  return ()

--- Types ---

data Height
  = CM Int
  | Inch Int

data Passport = Passport
  { byr :: Maybe PassportItems,
    iyr :: Maybe PassportItems,
    expyr :: Maybe PassportItems,
    hgt :: Maybe PassportItems,
    hcl :: Maybe PassportItems,
    ecl :: Maybe PassportItems,
    pid :: Maybe PassportItems,
    cid :: Maybe PassportItems
  }
  deriving (Eq, Show)

defaultPassport :: Passport
defaultPassport =
  Passport
    { byr = Nothing,
      iyr = Nothing,
      expyr = Nothing,
      hgt = Nothing,
      hcl = Nothing,
      ecl = Nothing,
      pid = Nothing,
      cid = Nothing
    }

data PassportItems
  = BirthYear T.Text
  | IssueYear T.Text
  | ExpYear T.Text
  | Height T.Text
  | HairColor T.Text
  | EyeColor T.Text
  | PassportId T.Text
  | CountryId T.Text
  deriving (Eq, Show)

---- Make and Validate Passport ----

makeAndValidatePassport :: [PassportItems] -> Passport
makeAndValidatePassport = foldl' step defaultPassport
  where
    step p (BirthYear by) = p {byr = validateBirthYear by}
    step p (IssueYear iy) = p {iyr = validateIssueYear iy}
    step p (ExpYear ey) = p {expyr = validateExpYear ey}
    step p (Height h) = p {hgt = validateHeight h}
    step p (HairColor hc) = p {hcl = validateHairColor hc}
    step p (EyeColor ec) = p {ecl = validateEcl ec}
    step p (PassportId id) = p {pid = validatePassportId id}
    step p b@(CountryId _) = p {cid = Just b}

--- Make passport ---

makePassport :: [PassportItems] -> Passport
makePassport = foldl' step defaultPassport
  where
    step p b@(BirthYear _) = p {byr = Just b}
    step p b@(IssueYear _) = p {iyr = Just b}
    step p b@(ExpYear _) = p {expyr = Just b}
    step p b@(Height _) = p {hgt = Just b}
    step p b@(HairColor _) = p {hcl = Just b}
    step p b@(EyeColor _) = p {ecl = Just b}
    step p b@(PassportId _) = p {pid = Just b}
    step p b@(CountryId _) = p {cid = Just b}

---- Validations ----

validateBirthYear :: T.Text -> Maybe PassportItems
validateBirthYear t = do
  guard (T.length t == 4)
  n <- readMaybe (T.unpack t) :: Maybe Int
  guard $ n >= 1920 && n <= 2002
  return $ BirthYear t

validateIssueYear :: T.Text -> Maybe PassportItems
validateIssueYear t = do
  guard (T.length t == 4)
  n <- readMaybe (T.unpack t) :: Maybe Int
  guard $ n >= 2010 && n <= 2020
  return $ IssueYear t

validateExpYear :: T.Text -> Maybe PassportItems
validateExpYear t = do
  guard (T.length t == 4)
  n <- readMaybe (T.unpack t) :: Maybe Int
  guard $ n >= 2020 && n <= 2030
  return $ ExpYear t

validateEcl :: T.Text -> Maybe PassportItems
validateEcl t = do
  guard $ t `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  return $ EyeColor t

validatePassportId :: T.Text -> Maybe PassportItems
validatePassportId t = do
  guard $ T.length t == 9
  readMaybe (T.unpack t) :: Maybe Integer
  return $ PassportId t

validateHeight :: T.Text -> Maybe PassportItems
validateHeight t = do
  h <- AT.maybeResult $ AT.parse parseHeight t
  case h of
    CM n -> do
      guard $ n >= 150 && n <= 193
      return $ Height t
    Inch n -> do
      guard $ n >= 59 && n <= 76
      return $ Height t

validateHairColor :: T.Text -> Maybe PassportItems
validateHairColor t = do
  guard $ T.length t == 7
  AT.maybeResult $ AT.parse parseHairColor t
  return $ HairColor t

parseHairColor :: Parser T.Text
parseHairColor = do
  h <- char '#'
  s <- AT.count 6 (satisfy $ AT.inClass "0-9a-f")
  return $ T.pack $ h : s

validPassport :: Passport -> Bool
validPassport Passport {..} =
  isJust byr && isJust iyr && isJust expyr && isJust hgt && isJust hcl && isJust ecl && isJust pid

parseHeight :: Parser Height
parseHeight =
  do
    n <- decimal
    string "cm"
    return $ CM n
    <|> do
      n <- decimal
      string "in"
      return $ Inch n

parseAllPassports :: Parser [[PassportItems]]
parseAllPassports = many1' (choice passportItemsParser) `sepBy` char '\n'

passportItemsParser :: [Parser PassportItems]
passportItemsParser =
  [issueYearParser, heightParser, birthYearParser, expirationYearParser, pidParser, cidParser, eyeColorParser, hairColorParser]

-- Parsers --

birthYearParser :: Parser PassportItems
birthYearParser = BirthYear <$> parseKeyValue "byr"

issueYearParser :: Parser PassportItems
issueYearParser = IssueYear <$> parseKeyValue "iyr"

expirationYearParser :: Parser PassportItems
expirationYearParser = ExpYear <$> parseKeyValue "eyr"

cidParser :: Parser PassportItems
cidParser = CountryId <$> parseKeyValue "cid"

pidParser :: Parser PassportItems
pidParser =
  PassportId
    <$> parseKeyValue "pid"

hairColorParser :: Parser PassportItems
hairColorParser =
  HairColor <$> do
    string "hcl:"
    s <- manyTill' anyChar spaceOrNewLine
    return $ T.pack s

eyeColorParser :: Parser PassportItems
eyeColorParser =
  EyeColor <$> do
    string "ecl:"
    s <- hexColorParser <|> AT.take 3
    spaceOrNewLine
    return s

heightParser :: Parser PassportItems
heightParser =
  Height <$> do
    string "hgt:"
    s <- manyTill' anyChar spaceOrNewLine
    return $ T.pack s

-- Helper Functions --

parseKeyValue :: T.Text -> Parser T.Text
parseKeyValue s = do
  string s
  char ':'
  s <- manyTill anyChar spaceOrNewLine
  return $ T.pack s

spaceOrNewLine :: Parser Char
spaceOrNewLine = do
  char '\n' <|> AT.satisfy isHorizontalSpace

hexColorParser :: Parser T.Text
hexColorParser = do
  c <- char '#'
  s <- AT.take 6
  return $ T.cons c s

---

p1 :: T.Text
p1 = "hgt:176cm\niyr:2013\nhcl:#fffffd ecl:amb\nbyr:2000\neyr:2034\ncid:89 pid:934693255\n\nhcl:#b5c3db ecl:grn hgt:155cm pid:#baec97 iyr:2017\nbyr:1939\neyr:2020\n\npid:526669252 eyr:1972\nhgt:152cm ecl:dne byr:1960 hcl:z iyr:2023\n\n"
