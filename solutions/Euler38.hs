import Data.List

try = [9876, 9875..5123]

digits = map cDoubles try
  where cDoubles n = show n ++ show (n * 2)

pandigits = filter pandigital digits
  where pandigital n = length n == 9 && not ('0' `elem` n) && length (nub n) == 9

main = putStrLn $ head pandigits