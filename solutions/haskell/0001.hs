-- current solution
-- (0.01 secs, 473,832 bytes)

sol = sum [ x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0 ]
