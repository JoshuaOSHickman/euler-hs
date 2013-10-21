import Data.List

prime :: Integer -> Bool
prime 1 = False
prime 2 = True
prime n = not . any (== 0) . map (mod n) $ [2..(ceiling . sqrt $ fromIntegral n)]

isTruncatablePrime n = fromBeginning && fromEnd
  where fromBeginning = all prime . map read . filter (not . null) . inits $ show n
        fromEnd = all prime . map read . filter (not . null) . tails $ show n

primes = filter prime [1..]

main = print . sum . drop 4 . take 15 . filter isTruncatablePrime $ primes