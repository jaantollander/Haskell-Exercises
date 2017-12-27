-- Problem 27
-- Group the elements of a set into disjoint subsets.

-- TODO:
combinations_with_residuals :: Int -> [a] -> [([a], [a])]
combinations_with_residuals k list = [([], [])]

-- [2, 2, 5], "ABCDEFGHI" ->
-- [ [[A, B], [C, D, E], [F, G, H, I, J]], ... ]

-- FIXME:
group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[[]]]
group _ [] = [[[]]]
group (k:ks) list =
  foldr [] (++) $ map (group' ks) (combinations_with_residuals k list)
  where
    group' :: [Int] -> ([a], [a]) -> [[[a]]]
    group' ks (combination, residual) = map (:combination) (group ks residual)
