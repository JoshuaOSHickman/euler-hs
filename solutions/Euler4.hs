-----------------------------------------------------------------------------
--
-- Module      :  Euler4
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

module Euler4 (
    main
) where

palindrome n = s == reverse s
                where s = show n


value = foldl max 0 [x * y | x <- [100..999],
                              y <- [100..999], palindrome (x * y)]


doShow = do
    (putStrLn . show) value

main = doShow
