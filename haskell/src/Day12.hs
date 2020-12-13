{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard)
import Data.Attoparsec.Text (Parser, anyChar, char, choice, decimal, isHorizontalSpace, many1', manyTill', parseOnly, satisfy, string)
import Data.List (find, splitAt, (!!))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import MyLib (countTrue)

data Ship = Ship
  { dir :: Direction,
    east :: Int,
    north :: Int
  }
  deriving (Eq, Show)

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Show)

instance Enum Direction where
  succ North = East
  succ East = South
  succ South = West
  succ West = North

  pred North = West
  pred West = South
  pred South = East
  pred East = North

defaultShip :: Ship
defaultShip = Ship East 0 0

tpartone :: IO ()
tpartone = do
  contents <- T.lines <$> TIO.readFile "../data/12.txt"
  example1 <- T.lines <$> TIO.readFile "./example-data/12.txt"
  let ec = traverse (parseOnly commandParser) contents
  case ec of
    Left e -> print e
    Right commands -> do
      let result = moveShip defaultShip commands
      print result
  return ()

charToCommand :: Char -> Int -> Commands
charToCommand 'N' = N
charToCommand 'S' = S
charToCommand 'E' = E
charToCommand 'W' = W
charToCommand 'L' = L
charToCommand 'R' = R
charToCommand 'F' = F

commandParser :: Parser Commands
commandParser = do
  c <- choice [char 'N', char 'S', char 'E', char 'W', char 'L', char 'R', char 'F']
  n <- decimal
  return $ charToCommand c n

moveShip :: Ship -> [Commands] -> Ship
moveShip s [] = s
moveShip s@(Ship dir e n) (c : cs) =
  case c of
    N k -> moveShip s {north = n + k} cs
    S k -> moveShip s {north = n - k} cs
    E k -> moveShip s {east = e + k} cs
    W k -> moveShip s {east = e - k} cs
    L k -> moveShip (turnLeft k s) cs
    R k -> moveShip (turnRight k s) cs
    F k -> moveShip (moveAhead k s) cs

data Commands
  = N Int
  | S Int
  | E Int
  | W Int
  | L Int
  | R Int
  | F Int
  deriving (Show)

turnLeft :: Int -> Ship -> Ship
turnLeft n s@(Ship dir _ _) =
  s {dir = iterate pred dir !! (n `div` 90)}

turnRight :: Int -> Ship -> Ship
turnRight n s@(Ship dir _ _) =
  s {dir = iterate succ dir !! (n `div` 90)}

moveAhead :: Int -> Ship -> Ship
moveAhead k s@(Ship dir e n) =
  case dir of
    North -> s {north = n + k}
    East -> s {east = e + k}
    West -> s {east = e - k}
    South -> s {north = n - k}

---- PART two ----

type Waypoint = Ship

defaultWayPoint = Ship East 10 1

twPart2 :: IO ()
twPart2 = do
  contents <- T.lines <$> TIO.readFile "../data/12.txt"
  example1 <- T.lines <$> TIO.readFile "./example-data/12.txt"
  let ec = traverse (parseOnly commandParser) contents
  case ec of
    Left e -> print e
    Right commands -> do
      print commands
      let result = moveShipWithWayPoint commands (defaultShip, defaultWayPoint)
      print result
  return ()

moveShipWithWayPoint :: [Commands] -> (Ship, Waypoint) -> (Ship, Waypoint)
moveShipWithWayPoint [] s = s
moveShipWithWayPoint (c : cs) (s@(Ship _ e n), w@(Ship _ we wn)) =
  case c of
    N k -> moveShipWithWayPoint cs (s, w {north = wn + k})
    S k -> moveShipWithWayPoint cs (s, w {north = wn - k})
    E k -> moveShipWithWayPoint cs (s, w {east = we + k})
    W k -> moveShipWithWayPoint cs (s, w {east = we - k})
    F k -> moveShipWithWayPoint cs (s {north = n + wn * k, east = e + we * k}, w)
    R k -> moveShipWithWayPoint cs (s, turnWaypointRight k w)
    L k -> moveShipWithWayPoint cs (s, turnWaypointLeft k w)
  where
    turnWaypointRight :: Int -> Waypoint -> Waypoint
    turnWaypointRight d w@(Ship _ e n)
      | d == 90 = w {north = - e, east = n}
      | d == 180 = w {north = - n, east = - e}
      | d == 270 = w {north = e, east = - n}
      | otherwise = w
    turnWaypointLeft :: Int -> Waypoint -> Waypoint
    turnWaypointLeft d w@(Ship _ e n)
      | d == 90 = w {north = e, east = - n}
      | d == 180 = w {north = - n, east = - e}
      | d == 270 = w {north = - e, east = n}
      | otherwise = w
