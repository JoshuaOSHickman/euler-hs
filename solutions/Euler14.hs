import Data.Function
import Data.List

nextCollatz n = if n `mod` 2 == 0
                then n `div` 2
                else 3 * n + 1

collatzLength n = collatzLength' n 0
                  where collatzLength' 1 l = l
                        collatzLength' n l = collatzLength' (nextCollatz n) (l + 1)

longestCollatz n = let lengths = map (\ i -> (i, collatzLength i)) [1..n]
                       highest = maximumBy (compare `on` snd) lengths
                   in fst highest

main = print $ longestCollatz 1000000