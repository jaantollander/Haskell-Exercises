-- Problem 27
-- Group the elements of a set into disjoint subsets.

type Set a = [a]

-- TODO:
-- | The function produces all combination of a set and a set of elements
-- that were not used in the combination.
combinations_with_residuals :: Int -> [a] -> [([a], [a])]
combinations_with_residuals k list = [([], [])]

-- [2, 3, 5], "ABCDEFGHI" ->
-- [ [[A, B], [C, D, E], [F, G, H, I, J]], ... ]
-- 2 "ABCDEFGHIJ" -> [("AB", "CDEFGHIJ"),...]
-- 3 "CDEFGHIJ" ->  [("CDE", "FGHIJ"),...]
-- 5 "FGHIJ" -> [("FGHIJ", ""),...]


-- FIXME:
-- | The function groups a set into disjoint subsets of sizes k.
group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[[]]] -- Recursion terminates here
group _ [] = [[[]]]
group (k:ks) list =
  let l = map (group' ks) (combinations_with_residuals k list)
  in foldr [] (++) l
  where
    group' :: [Int] -> ([a], [a]) -> [[[a]]]
    group' ks (combination, residual) = map (:combination) (group ks residual)
