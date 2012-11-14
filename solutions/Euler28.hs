ring :: (Enum a, Eq a, Num a) => a -> a
ring 0 = 1
ring n = let starting = (2 * n - 1) * (2 * n - 1)
             distanceBetweenCorners = 2 * n
         in sum $ map ((+ starting) . (* distanceBetweenCorners)) [1..4] 
        
rings :: (Enum b, Eq b, Num b) => b -> [b]
rings n = map ring [0..n]

main :: IO ()
main = print . sum . rings $ 500