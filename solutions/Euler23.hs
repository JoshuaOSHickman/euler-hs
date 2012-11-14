properDivisors :: Integral a => a -> [a]
properDivisors n = properDivisors' 2 [1]
  where 
    sqrtn = (+ 1) . floor . sqrt . fromIntegral $ n
    properDivisors' check acc
      | check < sqrtn, 
        n `mod` check == 0, 
        check /= (n `div` check) = properDivisors' (check + 1) ((n `div` check):check:acc)
      | check < sqrtn,
        n `mod` check == 0 = properDivisors' (check + 1) (check:acc)
      | check < sqrtn = properDivisors' (check + 1) acc
      | otherwise = acc

divisorSum :: Integer -> Integer
divisorSum = sum . properDivisors

abundant :: Integer -> Bool
abundant n = n < divisorSum n

abundantNumbers :: [Integer]
abundantNumbers = filter abundant [10..upperLimit]

upperLimit :: Integer
upperLimit = 28123

canBeSum :: Integer -> Bool
canBeSum n = canBeSum' abundantNumbers (reverse abundantNumbers)
  where canBeSum' increasing decreasing = 
          (increasing /= []) && (decreasing /= []) &&
          let lowestPossible = head increasing
              highestPossible = head decreasing
              s = lowestPossible + highestPossible
          in (n == s) || if n < s -- if too big, get smaller, else get larger
                         then canBeSum' increasing (tail decreasing)
                         else canBeSum' (tail increasing) decreasing
  
cannotBeSum :: [Integer]
cannotBeSum = filter (not . canBeSum) [1..upperLimit]

main :: IO ()
main = print $ sum cannotBeSum