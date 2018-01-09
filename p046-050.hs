-- Problem 46
--  Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for
-- logical equivalence) which succeed or fail according to the result of their
-- respective operations; e.g. and(A,B) will succeed, if and only if both A and
-- B succeed.

-- https://en.wikibooks.org/wiki/Digital_Circuits/Logic_Operations

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

not' :: Bool -> Bool
not' True = False
not' False = True

-- not' (and' a b)
nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _ _ = True

-- not' (or' a b)
nor' :: Bool -> Bool -> Bool
nor' False False = True
nor' _ _ = False

-- and' (not' a) b
xor' :: Bool -> Bool -> Bool
xor' True True = False
xor' False False = False
xor' _ _ = True

-- not' xor' a b
xnor' :: Bool -> Bool -> Bool
xnor' True True = True
xnor' False False = True
xnor' _ _ = False

-- Print truth table
table' :: (Bool -> Bool -> Bool) -> String
table' predicate = unlines [unwords (map show [a, b, predicate a b]) | a <- [True, False], b <- [True, False]]

table :: (Bool -> Bool -> Bool) -> IO ()
table predicate = putStrLn (table' predicate)


-- Problem 48
-- Generalize problem P47 in such a way that the logical expression may contain
-- any number of logical variables. Define table/2 in a way that
-- table(List,Expr) prints the truth table for the expression Expr, which
-- contains the logical variables enumerated in List.

-- |
-- genTable 1 [True, False]
-- [[True], [False]]
--
-- genTable 2 [True, False]
-- [True:[True], True:[False]] ++ [False:[True], False:[False]]
genTable :: Int -> [a] -> [[a]]
genTable 1 list = [[x] | x <- list]
genTable n list =
  let t' = genTable (n-1) list
  in foldr (++) [] [map (x:) t' | x <- list]

tablen' :: Int -> ([Bool] -> Bool) -> String
tablen' n predicate = unlines [(unwords (map show input)) ++ " " ++ (show (predicate input)) | input <- (genTable n [True, False]) ]

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n predicate = putStrLn (tablen' n predicate)
