

main :: IO ()
main = do
  gridData <- readFile "../data/grid11.txt"
  let grid = map (map read . words) . lines $ gridData
      positions = [(x, y) | x <- [0..19], y <- [0..19]]
      offsets = [(dx, dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]
      permutations = [map (\ off -> (x + dx * off, y + dy * off)) [0..3] | 
                      (x, y) <- positions, (dx, dy) <- offsets]
      valid (x, y) = 0 <= x && x <= 19 && 0 <= y && y <= 19
      indices = filter (all valid) permutations
      grab (x, y) = (grid !! y) !! x
      combinations = map (map grab) indices
      products = map product combinations
  print (foldl max 0 products)
  