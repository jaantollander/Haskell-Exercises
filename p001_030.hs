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


-- Problem 8
-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single
-- copy of the element. The order of the elements should not be changed.
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
  | x == y    = compress (y:xs)
  | otherwise = [x] ++ compress (y:xs)


-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. If a list
-- contains repeated elements they should be placed in separate sublists.

-- Splits the list into a pair of lists where all the elements in the first
-- list are equal.
-- "aaaa" -> ("aaaa", "")
-- "aaaabccaaddeeee" -> ("aaaa", "bccaaddeeee")
split' :: Eq a => ([a], [a]) -> ([a], [a])
split' ([], []) = ([], [])
split' ([], (y:ys)) = split' ([y], ys)
split' (xs, []) = (xs, [])
split' ((x:xs), (y:ys))
  | x == y    = split' ((x:y:xs), ys)
  | otherwise = ((x:xs), (y:ys))

-- Apply split recursively
--  list = "aaaabccaaddeeee"
--      split' (list, []) -> ("aaaa", "bccaaddeeee")
--   -> ("aaaa", "b", "ccaaddeeee")
--   -> ...
--   -> ("aaaa", "b", "cc", "aa", "dd", "eeee")
pack :: Eq a => [a] -> [[a]]
pack list = pack' $ split' ([], list)
  where
    pack' (h, []) = [h]
    pack' (h, t) = [h] ++ pack(t)


-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 to implement the
-- so-called run-length encoding data compression method. Consecutive duplicates
-- of elements are encoded as lists (N E) where N is the number of duplicates of
-- the element E.
encode :: Eq a => [a] -> [(Int, a)]
encode [] = error ""
encode list = map (\x -> (length x, head x)) (pack list)


-- Problem 11
-- Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.
data Encoding a = Single a | Multiple Int a deriving (Show)
makeEncoding :: Int -> a -> Encoding a
makeEncoding num item
  | num == 1  = Single item
  | otherwise = Multiple num item
encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified list = map (\x -> makeEncoding (length x) (head x)) (pack list)


-- Problem 12
-- Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct
-- its uncompressed version.
unmakeEncoding :: Encoding a -> [a]
unmakeEncoding (Single item) = [item]
unmakeEncoding (Multiple num item) = [item | _ <- [1..num]]

decodeModified :: [Encoding a] -> [a]
decodeModified list = foldr (++) [] (map unmakeEncoding list)


-- Problem 13
-- Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in
-- problem 9, but only count them. As in problem P11, simplify the result list
-- by replacing the singleton lists (1 X) by X.
encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect [] = []
encodeDirect list =
  let (encoding, list_tail) = encodeDirect' list
  -- Recursion terminates when list_tail is empty list.
  in encoding:(encodeDirect list_tail)

  where
    encodeDirect'' :: Eq a => Int -> a -> [a] -> (Int, a, [a])
    encodeDirect'' num e [] = (num, e, [])
    encodeDirect'' num e (x:xs)
       | e == x    = encodeDirect'' (num + 1) e xs
       | otherwise = (num, e, (x:xs))

    -- "aaaabccaadeeee" -> (4, 'a'), "bbccaaddeeee"
    encodeDirect' :: Eq a => [a] -> (Encoding a, [a])
    encodeDirect' (x:xs) =
      let (num, y, ys) = encodeDirect'' 1 x xs
      in (makeEncoding num y, ys)


-- Problem 14
-- Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x, x] ++ dupli xs



-- Problem 15
-- Replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) num = [x | _ <- [1..num]] ++ repli xs num


-- Problem 16
-- Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery list n = [e | (e, i) <- zip list [1..], mod i n /= 0]


-- Problem 17
-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split list n =
  let (pair, _) = split' ([], list) n
  in pair
  where
    split' :: ([a], [a]) -> Int -> (([a], [a]), Int)
    split' pair 0 = (pair, 0)
    split' (h, []) m = ((h, []), m)
    split' (h, (x:xs)) m = split' (h ++ [x], xs) (m - 1)


-- Problem 18
slice :: [a] -> Int -> Int -> [a]
slice list n m
  | n > m  = error "n shouldn't be larger than m"
  | n < 0  = error "n and m should be positive "
  | otherwise = drop (n - 1) (take m list)


-- Problem 19
-- Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate list 0 = list
rotate list num
  | num < 0   = rotate list (l - (mod (-num) l))
  | num >= l  = rotate list (mod num l)
  | otherwise =
    let (h, t) = split list num
    in t ++ h
  where
    l = length list


-- Problem 20
-- Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "Cannot remove element from empty list."
removeAt i list@(e:_)
  | i < 1     = error "Index out of bounds."
  | i > l     = error "Index out of bounds."
  | otherwise = removeAt' i e list
  where
    l = length list
    removeAt' :: Int -> a -> [a] -> (a, [a])
    removeAt' i e [] = (e, [])
    removeAt' i e (x:xs)
      | i == 1 =
        let (e2, list) = removeAt' (i - 1) x xs
        in (e2, list)
      | otherwise =
        let (e2, list) = removeAt' (i - 1) e xs
        in (e2, x:list)


-- Problem 21
-- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt _ [] _ = []
insertAt element (x:xs) index
  | index == 1 = element:x:(insertAt element xs (index - 1))
  | otherwise  = x:(insertAt element xs (index - 1))
