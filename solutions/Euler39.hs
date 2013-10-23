import Data.Function
import Data.List

-- odds square to odd, evens square to evens. 
-- for any two of the sides, we see that if they are both even, their sum is even, and therefore must be the square of an even, therefore the perimeter is even. 
-- if one is odd, they sum to an odd, z is odd, so perimeter is even.
-- if both are odd, they sum to an even and the perimeter is even. 

numberSolutions n = length [(x, y, z) | x <- [1..n], y <- [1..x], let z = n - (x + y), z * z == x * x + y * y]

-- surely if it's below 500, we could multiply all the solutions by 2 and get at least that many solutions in the 501-100 band
solutions = map sols [2, 4..1000]
  where sols n = (n, numberSolutions n)

main = print $ maximumBy (compare `on` snd) solutions
