-- Problem 31
-- Determine whether a given integer number is prime.

-- div 7 2 = 3
-- mod 7 3 = 1
-- mod 7 2 = 1

-- | https://en.wikipedia.org/wiki/Primality_test#Pseudocode
isPrime :: Integral a => a -> Bool
isPrime n
  | n <= 1 = False
  | n <= 3 = True
  | ((mod n 2) == 0) || ((mod n 3) == 0) = False
  | otherwise = primetest n 5
  where
    primetest :: Integral a => a -> a -> Bool
    primetest n i
      | i^2 > n = True
      | ((mod n i) == 0) || ((mod n (i + 2)) == 0) = False
      | otherwise = primetest n (i+6)


-- Problem 32
-- Determine the greatest common divisor of two positive integer numbers. Use
-- Euclid's algorithm.
-- https://en.wikipedia.org/wiki/Euclidean_algorithm
myGCD :: Integral a => a -> a -> a
myGCD a b
  | b == 0 = a
  | otherwise = myGCD b (a `mod` b)


-- Problem 33
-- Determine whether two positive integer numbers are coprime. Two numbers are
-- coprime if their greatest common divisor equals 1.
coprime :: Integral a => a -> a -> Bool
coprime a b = gcd a b == 1


-- Problem 34
-- Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number of
-- positive integers r (1 <= r < m) that are coprime to m.
totient :: Integral a => a -> a
totient 1 = 1
totient m = sum [value (coprime r m) | r <- [1..(m-1)]]
  where
    value :: Integral a => Bool -> a
    value a
      | a = 1
      | otherwise = 0


-- Problem 35
-- Determine the prime factors of a given positive integer. Construct a flat
-- list containing the prime factors in ascending order.
