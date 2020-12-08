module AOCStarterTemplate where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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