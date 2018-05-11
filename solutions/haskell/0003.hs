isFactorOf :: Int -> Int -> Bool
isFactorOf x y
 | x == y = True
 | x < y = False
 | otherwise = mod x y == 0

isFactorOfAny :: Int -> [Int] -> Bool
isFactorOfAny _ [] = False
isFactorOfAny n (x:xs) = isFactorOf n x || isFactorOfAny n xs

-- Story of my infinite list of primes.

-- While searching for algorithms to finding primes numbers,
-- I come up to well known ancient algorithm which called "Sieve of Eratosthenes".
-- https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

-- Inspired from it, i come up with an idea of algorithm that checks the primality
-- of x by testing its divisibility by each of the primes less than x.
-- Afterwards i learn that this algorithm which i "come up with" is already
-- a known algorithm which known as trial division. Although it takes week for me
-- to came out with this idea; they called this "a simple naive algorithm." (FML)
-- https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
primes :: [Int]
primes = 2:3:[
  x | x <- [5,7..], not (
    isFactorOfAny x (
      takeWhile (\a -> a*a <= x) primes
    )
  )]

findLargestPrime' :: Int -> Int -> Int
findLargestPrime' 1 _ = 0
findLargestPrime' n pin
  | modr == 0 && divr == 1 = prime
  | modr == 0 = findLargestPrime' divr pin
  | otherwise = findLargestPrime' n (pin + 1)
  where prime = (primes !! pin)
        modr = mod n prime
        divr = div n prime

findLargestPrime :: Int -> Int
findLargestPrime n = findLargestPrime' n 0

-- current solution
-- (0.04 secs, 13,335,864 bytes)

sol = findLargestPrime 600851475143
