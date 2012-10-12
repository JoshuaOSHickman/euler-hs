module Euler9 (main) where 

value = head [a * b * (1000 - b - a) | a <- [1..1000], b <- enumFromTo 1 (1000 - a),
           a * a + b * b == (1000 - a - b) * (1000 - a - b)]

main = do
  putStrLn . show $ value