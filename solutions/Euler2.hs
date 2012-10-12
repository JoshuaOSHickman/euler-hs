-----------------------------------------------------------------------------
--
-- Module      :  Euler2
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

module Euler1 (
    main
) where

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

value = (foldl (+) 0) . (filter even) . (takeWhile (<= 4000000)) $ fibs


doShow = do
    (putStrLn . show) value

main = doShow


