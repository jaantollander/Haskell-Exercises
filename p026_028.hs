-- Problem 26
-- Generate the combinations of K distinct objects chosen from the N elements
-- of a list
-- https://en.wikipedia.org/wiki/Combination
combinations :: Int -> [a] -> [[a]]
combinations _ [] = error ""
combinations k list@(x:xs)
  | k == 0 = [[]]
  | k == 1 = [[x] | x <- list]
  | k == l = [list]
  | k >= l = error ""
  | otherwise =
    [x:c | c <- (combinations (k - 1) xs)] ++ (combinations k xs)
  where
    l = length list
