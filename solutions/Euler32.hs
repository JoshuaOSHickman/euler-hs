import qualified Data.Set as Set
import Data.Set(Set)
import Data.List(sort)

options1 = [10..98]
options2 = [10..999]

options = [2..10000]

multipliedDigits x y = digits x ++ digits y ++ digits (x * y)
  where digits n = if n == 0
                   then []
                   else (n `mod` 10) : (digits (n `div` 10))

duplicates xs = duplicates' ns
  where ns = sort xs
        duplicates' [] = False
        duplicates' (x:[]) = False
        duplicates' (x:(y:xs)) | x == y = True
        duplicates' (x:(y:xs)) = duplicates' (y:xs)

pandigits digits = not (0 `elem` digits) && length digits == 9 && not (duplicates digits)

selections = [x * y | x <- options, y <- options, x < y, pandigits (multipliedDigits x y)]

main = print . sum . Set.toList . Set.fromList $ selections