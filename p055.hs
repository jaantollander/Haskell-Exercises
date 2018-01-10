-- https://en.wikipedia.org/wiki/Binary_tree
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)


-- Problem 55
cbalTree :: Int -> Tree Char
cbalTree n
  | n > 0 = Branch 'x' (cbalTree m) (cbalTree m')
  | otherwise = Empty
  where
    n' = n - 1
    m = div n' 2
    m' = n' - m
