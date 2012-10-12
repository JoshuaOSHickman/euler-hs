

main = do
  numberData <- readFile "../data/number13.txt"     
  let numbers = map (read :: String -> Integer) (lines numberData)
      s = sum numbers
      m = take 10 (show s)
  putStrLn m