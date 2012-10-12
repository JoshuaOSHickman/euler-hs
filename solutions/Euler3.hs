-----------------------------------------------------------------------------
--
-- Module      :  Euler3
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

module Euler3 (

) where

largePrime :: Integer
largePrime = 600851475143

nextPrimeFactor n minimum = if 0 == rem n minimum
                            then minimum
                            else nextPrimeFactor n (1 + minimum)

withoutFactor n f = if 0 == rem n f
                    then withoutFactor (div n f) f
                    else n

primeFactors n minimum = if n == 1
                 then []
                 else factor : primeFactors (withoutFactor n factor) (1 + factor)
                    where factor = nextPrimeFactor n minimum

value = primeFactors largePrime 2

doShow = do
    (putStrLn . show) value

main = doShow
