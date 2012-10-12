-----------------------------------------------------------------------------
--
-- Module      :  Euler1
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


value = foldl (+) 0 (filter (\x -> 0 == rem x 3 || 0 == rem x 5) [1..999])

doShow = do
    (putStrLn . show) value

main = doShow
