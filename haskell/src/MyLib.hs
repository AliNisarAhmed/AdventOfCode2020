module MyLib where

count :: Eq a => a -> [a] -> Int
count a = length . filter (== a)

countTrue :: [Bool] -> Int
countTrue = count True

infix 1 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x