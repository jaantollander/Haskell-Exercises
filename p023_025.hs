import System.Random (RandomGen, randomR, mkStdGen)

-- Problem 23
-- Extract a given number of randomly selected elements from a list.


-- | Select one element from list and return a pair of the element
-- and non-selected elements.
-- >>> select_one "12345" 1
-- ('2', "1345")
select_one :: [a] -> Int -> (a, [a])
select_one [] _ = error "Can't select one from empty list."
select_one (x:xs) 0 = (x, xs)
select_one (x:xs) index =
  let (selected, not_selected) = select_one xs (index - 1)
  in (selected, x:not_selected)


-- | Select one random element from a list.
rnd_select_one :: RandomGen g => g -> [a] -> ((a, [a]), g)
rnd_select_one gen list =
  let (index, gen') = randomR (0, ((length list) - 1)) gen
  in (select_one list index, gen')


-- | Extract a given number of randomly selected elements from a
-- list.
rnd_select :: RandomGen g => g -> [a] -> Int -> [a]
rnd_select _ _ 0 = []
rnd_select _ [] _ = error ""
rnd_select gen list num =
  let ((selected, not_selected), gen') = rnd_select_one gen list
  in selected:(rnd_select gen' not_selected (num - 1))


-- Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.
diff_select :: RandomGen g => g -> Int -> Int -> [Int]
diff_select gen n m = rnd_select gen [1..m] n


-- Problem 25
-- Generate a random permutation of the elements of a list.
rnd_permu :: RandomGen g => g -> [a] -> [a]
rnd_permu gen list = rnd_select gen list (length list)
