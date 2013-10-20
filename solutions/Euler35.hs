import qualified Data.Set as Set
import Data.Set (Set)
import Data.List(tails)

union (x:xs) (y:ys) = case compare x y of
  LT -> x : union  xs (y:ys)
  EQ -> x : union  xs    ys
  GT -> y : union (x:xs) ys
union a [] = a
union [] b = b

minus (x:xs) (y:ys) = case compare x y of
  LT -> x : minus  xs (y:ys)
  EQ ->     minus  xs    ys
  GT ->     minus (x:xs) ys
minus a b = a

primes = 2 : ([3,5..] `minus` unionAll [[p*p, p*p+2*p..] | p <- primes'])
  where
    primes' = 3 : ([5,7..] `minus` unionAll [[p*p, p*p+2*p..] | p <- primes'])
    unionAll ((x:xs):t) = x : union xs (unionAll (pairs t))
    pairs ((x:xs):ys:t) = (x : union xs ys) : pairs t

eligiblePrimes = takeWhile (< 1000000) primes

sPrimes = Set.fromList eligiblePrimes
isPrime n = Set.member n sPrimes

rotate xs = tail xs ++ [head xs]

rotations :: Integer -> [Integer]
rotations prime = map read . take (length digits) . iterate rotate $ digits
  where 
    digits = show prime

circular prime = all isPrime (rotations prime)

main = print . length . filter circular $ eligiblePrimes
