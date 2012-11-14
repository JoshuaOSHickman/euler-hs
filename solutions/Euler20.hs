fact :: (Eq a, Num a) => a -> a
fact n = fact' n 1
  where fact' 1 acc = acc
        fact' n acc = fact' (n - 1) (acc * n)

digitsF :: Integer -> [Integer]
digitsF = map (read . (: [])) . show . fact

main = print . sum . digitsF $ 100