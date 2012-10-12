
sumDigits :: Integer -> Integer
sumDigits n = sum $ map (read . (: [])) $ show n


main = print (sumDigits (2 ^ 1000))