-- some general insights:
 -- b must be positive and prime for n = 0
 -- a must be selected from numbers where b - 1 + a = p, where p is another prime
 -- this means the primes we need to check are bounded between 0 and 2000

-- in terms of checks for primes, might be easier to not enumerate primes, as the sequences might get quite high

-- NOTE: compile with -rtsopts and run with +RTS -K100M -RTS to avoid heap issues

import Data.List
import Data.Function(on)

quadLen :: Integral b => b -> b -> b
quadLen a b = qseries 0
  where qseries n = 
          if prime ((n * n) + (a * n) + b)
          then qseries (n + 1)
          else n
        
prime :: Integral b => b -> Bool
prime 1 = False
prime 2 = True
prime 3 = True
prime n = not $ any ((== 0) . (n `mod`)) [2..sqrtn]
  where sqrtn = (+ 1) . ceiling . sqrt . fromIntegral $ n

bestPair :: (Integer, Integer)
bestPair = nextBest (-1000) 1 1 1 0
  where nextBest 1000 1000 besta bestb bestscore = (besta, bestb)
        nextBest a 1000 besta bestb bestscore = nextBest (a + 1) 1 besta bestb bestscore
        nextBest a b besta bestb bestscore  
          -- don't care about non-prime b (won't match when n=0
          | not . prime $ b = nextBest a (b + 1) besta bestb bestscore
        nextBest a b besta bestb bestscore  
          -- example says it must be more than 40 long, as it beats euler, and negative numbers can't be prime. 
          | b < (-1600) - (40 * a) = nextBest a (b + 1) besta bestb bestscore
        nextBest a b besta bestb bestscore 
          -- at b, all terms are divisible by b, so that's the max score it will produce
          | b < bestscore = nextBest a (b + 1) besta bestb bestscore
        nextBest a b besta bestb bestscore = let l = quadLen a b
                                             in if l > bestscore
                                                then nextBest a (b + 1) a b l
                                                else nextBest a (b + 1) besta bestb bestscore


main :: IO ()
main = do
  let (a, b) = bestPair
  print (a * b)

