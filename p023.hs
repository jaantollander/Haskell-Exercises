import System.Random (Stdgen, mkStdGen, random, randomR)


-- Problem 23
-- Extract a given number of randomly selected elements from a list.

-- select_one "12345", 1 -> ('2', "1345")
select_one :: [a] -> Int -> (a, [a])
select_one [] _ = error "Can't select one from empty list."
select_one (x:xs) 0 = (x, xs)
select_one (x:xs) index =
    let (selected, not_selected) = select_one xs (index - 1)
    in (selected, x:not_selected)

rnd_select_one :: StdGen -> [a] -> (a, [a])
rnd_select_one gen list =
  let (g, index) = randomR gen 0 (length list)
  in select_one list index

rnd_select :: StdGen -> [a] -> Int -> [a]
rnd_select _ _ 0 = []
rnd_select _ [] _ = error ""
rnd_select gen list num =
    let (selected, not_selected) = rnd_select_one gen list
    in selected:(rnd_select gen not_selected (num - 1))
