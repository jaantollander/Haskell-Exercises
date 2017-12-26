-- Problem 26
-- Generate the combinations of K distinct objects chosen from the N elements
-- of a list
combinations :: Int -> [a] -> [[a]]
combinations _ [] = error ""
combinations 1 list = map (\x -> [x]) list
combinations n list
  | n == (length list) = [list]
  | otherwise =
    let (h, t) = splitAt (n - 1) list
    in (map (\x -> h ++ [x]) t) ++ (combinations n (tail list))
