squareRoot :: Integer -> Integer
squareRoot = floor . sqrt . fromIntegral

numberFactors n = let potentials = takeWhile (< squareRoot n) [1..n]
                      factors = filter ((== 0) . (n `mod`)) potentials 
                  in 2 * length factors

triangles = map (\n -> n * (n + 1) `div` 2) [1..]

answer = head $ dropWhile ((<= 500) . numberFactors) triangles

main :: IO ()
main = print answer