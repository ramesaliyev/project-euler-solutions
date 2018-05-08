{--| 

# SOLUTION 1
  -- (26.60 secs, 12,820,990,448 bytes)
  
  fibonacci :: Int -> Int
  fibonacci 0 = 0;
  fibonacci 1 = 1;
  fibonacci n = fibonacci(n-1) + fibonacci(n-2)

  fibTakeWhileLower n = [ fibonacci x | x <- takeWhile (\x -> (fibonacci x) < n) [1..] ]

  sol = sum [ x | x <- fibTakeWhileLower 4000000, even x]
#

|--}

-- Current Solution
-- (0.01 secs, 88,728 bytes)

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

fibTakeWhileLower n = takeWhile (<n) fibs

sol = sum [ x | x <- fibTakeWhileLower 4000000, even x]
