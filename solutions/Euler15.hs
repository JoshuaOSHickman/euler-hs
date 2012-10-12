
-- the pattern I'm thinking is: 
-- do I see pascal's triangle in there?
-- 70 35 15 5  1
-- 35 20 10 4  1
-- 15 10 6  3  1
-- 5  4  3  2  1
-- 1  1  1  1  1 
-- answer is yes, this is pascal's triangle

-- for the 20 by twenty spot we need item 20 in row 40, I believe. Maybe 21. 

fact :: Integer -> Integer
fact n = product [1..n]

choose n r = fact n `div` (fact r * fact (n - r))

main = print (choose 40 20)