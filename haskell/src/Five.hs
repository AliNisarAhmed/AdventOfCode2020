module Five where

import Data.Foldable (Foldable (foldl'))

fivePartOne :: IO ()
fivePartOne = do
  contents <- lines <$> readFile "../data/5.txt"
  let result = maximum $ calcSeatId . calculateRowsAndColumns <$> contents
  print result
  return ()

fivePartTwo :: IO ()
fivePartTwo = do
  contents <- lines <$> readFile "../data/5.txt"
  let seats = calculateRowsAndColumns <$> contents
      seatIds = calcSeatId <$> seats
      xs = calcMySeatId seats seatIds
  print xs
  return ()

lowerHalf :: (Int, Int) -> (Int, Int)
lowerHalf (n1, n2) = (n1, (n1 + n2) `div` 2)

upperHalf :: (Int, Int) -> (Int, Int)
upperHalf (n1, n2) = (1 + (n1 + n2) `div` 2, n2)

calculateRowsAndColumns :: String -> (Int, Int)
calculateRowsAndColumns = rowAndColumn . foldl' step ((0, 127), (0, 7))
  where
    step (r, c) x =
      case x of
        'B' -> (upperHalf r, c)
        'F' -> (lowerHalf r, c)
        'R' -> (r, upperHalf c)
        'L' -> (r, lowerHalf c)
        _ -> error "Not possible if data is valid"

calcSeatId :: (Int, Int) -> Int
calcSeatId (r, c) = 8 * r + c

calcMySeatId :: [(Int, Int)] -> [Int] -> [Int]
calcMySeatId seats seatIds =
  [ mySeatId (r1, c1)
    | (r1, c1) <- seats,
      (r2, c2) <- seats,
      calcSeatId (r1, c1) + 1 == calcSeatId (r2, c2) - 1,
      calcSeatId (r1, c1) + 1 `notElem` seatIds
  ]
  where
    mySeatId (x, y) = 8 * x + y + 1

rowAndColumn :: ((Int, Int), (Int, Int)) -> (Int, Int)
rowAndColumn ((r, _), (c, _)) = (r, c)