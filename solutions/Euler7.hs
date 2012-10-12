-----------------------------------------------------------------------------
--
-- Module      :  Euler7
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Euler7 (
    main
) where

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

value = primes !! 10000

main = do
    (putStrLn . show) value
