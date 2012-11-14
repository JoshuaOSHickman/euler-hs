fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
value = sum . filter even . takeWhile (<= 4000000) $ fibs
main = print value



