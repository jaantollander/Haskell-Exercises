-- Problem 1
myLast :: [a] -> a
myLast [] = error ""
myLast [x] = x
myLast l = l !! (length l - 1)

-- Problem 2
myButLast :: [a] -> a
myButLast [] = error ""
myButLast [x] = error ""
myButLast l = l !! (length l - 2)

-- Problem 3
elementAt :: [a] -> Int -> a
elementAt l i = l !! (i - 1)

-- Problem 4
incrementer :: Int -> a -> Int
incrementer x _ = x + 1
myLength :: [a] -> Int
myLength l = foldl incrementer 0 l

-- Problem 5
reverse' :: [a] -> [a] -> [a]
reverse' h [] = h
reverse' h t = reverse' (head t : h) (tail t)

myReverse :: [a] -> [a]
myReverse l = reverse' [] l


-- Problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l | length l <= 3 = head(l) == last(l)
               | otherwise = isPalindrome(tail (init l))

-- Problem 7
-- data NestedList a = Elem a | List [NestedList a]
-- myFlatten :: NestedList a -> [a]
