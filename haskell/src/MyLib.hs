module MyLib where

count :: Eq a => a -> [a] -> Int
count a = length . filter (== a)