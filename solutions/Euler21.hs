import Data.Maybe(isJust)

properDivisors :: Integral a => a -> [a]
properDivisors n = properDivisors' 2 [1]
  where 
    sqrtn = floor . sqrt . fromIntegral $ n
    properDivisors' check acc
      | check < sqrtn, n `mod` check == 0 = properDivisors' (check + 1) ((n `div` check):check:acc)
      | check < sqrtn = properDivisors' (check + 1) acc
      | otherwise = acc

-- always returns (x, y) s.t. x < y
amicablePair :: Integral t => t -> Maybe (t, t)
amicablePair n = let spd = sum (properDivisors n)
                 in if n >= spd
                    then Nothing
                    else if n == sum (properDivisors spd)
                         then Just (n, spd)
                         else Nothing

amicablePairsBelow :: Integral t => t -> [(t, t)]
amicablePairsBelow n = [(x, y) | a <- [2..n], 
                        let pair = amicablePair a, 
                        isJust pair, 
                        let Just (x, y) = pair]

sumAmicableNumbersBelow :: Integral a => a -> a
sumAmicableNumbersBelow n = let pairs = amicablePairsBelow n
                                pairSum = map (uncurry (+)) pairs
                            in sum pairSum

main = print $ sumAmicableNumbersBelow 10000
