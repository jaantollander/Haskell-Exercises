-- Problem 1
-- Find the last element of a list.
-- Takes the tail of the list until one element left.
myLast :: [a] -> a
myLast [] = error ""
myLast [x] = x
myLast (_:xs) = myLast xs


-- Problem 2
-- Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error ""
myButLast [x] = error ""
myButLast [x1, x2] = x1
myButLast (_:xs) = myButLast xs


-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt [] _ = error ""
elementAt (x:xs) k | k == 1    = x
                   | k < 1     = error ""
                   | otherwise = elementAt xs (k - 1)


-- Problem 4
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs


-- Problem 5
-- Reverse a list.
myReverse :: [a] -> [a]
myReverse list = reverse' [] list
  where
    reverse' out [] = out
    reverse' out (x:xs) = reverse' (x : out) xs


-- Problem 6
-- Find out whether a list is a palindrome. A palindrome can be read forward or
-- backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome list | head(list) /= last(list) = False
                  | otherwise = isPalindrome (tail (init list))


-- Problem 7
-- Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a `flat' list by
-- replacing each list with its elements (recursively).
data NestedList a = Elem a | List [NestedList a] deriving (Show)
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten' xs
  where
    flatten' :: [NestedList a] -> [a]
    flatten' [] = []
    flatten' (x:xs) = flatten x ++ flatten' xs
