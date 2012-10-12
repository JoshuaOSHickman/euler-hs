-----------------------------------------------------------------------------
--
-- Module      :  Euler6
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

module Euler6 (
    main
) where

sumSquares lst = foldl (+) 0 (map (\x -> x * x) lst)
squareSum lst = s * s where s = foldl (+) 0 lst

value = squareSum n - sumSquares n where n = [1..100]

main = do
    (putStrLn . show) value

