-- Problem 27
-- Group the elements of a set into disjoint subsets.
combinations_with_residuals :: Int -> [a] -> [([a], [a])]
combinations_with_residuals k list = ...

-- [2, 2, 5], "ABCDEFGHI" ->
-- FIXME:
group :: [Int] -> [a] -> [[[a]]]
group (k:ks) list =
  [combination:(group ks unused) |
   (combination, unused) <- (combinations_with_residuals k list)]
