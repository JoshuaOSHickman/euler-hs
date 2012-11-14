import Data.List
import Data.Function(on)

decimalDigits n = digits 1
  where digits spillOver = let (d, m) = spillOver `divMod` n
                           in (d, m) : digits (10 * m)


loopLength l n = loopLength' startingList (tail startingList) 0
  where startingList = drop n l
        loopLength' tortoise hare stepsTaken = 
          if head tortoise == head hare
          then stepsTaken + 1
          else loopLength' (tail tortoise) (tail (tail hare)) (stepsTaken + 1)
        
loop n = loopLength (decimalDigits n) n

main = print $ maximumBy (compare `on` loop) [1..1000]