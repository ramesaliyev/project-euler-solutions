-- We can also develop a integer reverser,
-- which will take mod10 of first number recursively,
-- and reverse number. Then compare actual number
-- with reversed one. But since being palindromic
-- is more lexical thing than mathematical,
-- i think we dont need to.
isPalindromic x = stringified == reversed
  where stringified = show x
        reversed = reverse stringified

-- current solution
-- (0.37 secs, 317,089,816 bytes)

sol = maximum [ x |
    a <- [999,998..100],
    b <- [999,998..a],
    let x = a * b,
    isPalindromic x
  ]
