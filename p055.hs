-- https://en.wikipedia.org/wiki/Binary_tree
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)


-- Problem 55
-- TODO: generate all permutations
cbalTree :: Int -> Tree Char
cbalTree n
  | n > 0 = Branch 'x' (cbalTree m) (cbalTree m')
  | otherwise = Empty
  where
    n' = n - 1
    m = div n' 2
    m' = n' - m


-- Problem 56
-- Symmetric binary trees
-- Let us call a binary tree symmetric if you can draw a vertical line through
-- the root node and then the right subtree is the mirror image of the left
-- subtree. Write a predicate symmetric/1 to check whether a given binary tree
-- is symmetric. Hint: Write a predicate mirror/2 first to check whether one
-- tree is the mirror image of another. We are only interested in the
-- structure, not in the contents of the nodes.
symmetric :: Eq a => Tree a -> Bool
symmetric Empty = True
-- TODO: maybe try counting the amount of left and right nodes
symmetric (Branch a left right) = False
