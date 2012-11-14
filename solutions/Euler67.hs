maxStep :: (Num b, Ord b) => [b] -> [b] -> [b]
maxStep biggerLine smallerLine = let smallerLineWithIndex = zip smallerLine [0..(length smallerLine - 1)]
                                 in map (\(c, i) -> 
                                          let left = biggerLine !! i
                                              right = biggerLine !! (i + 1)
                                          in if left > right
                                             then left + c
                                             else right + c) smallerLineWithIndex

maxPath :: (Num a, Ord a) => [[a]] -> a
maxPath triangle = head $ foldl maxStep init combining
  where r = reverse triangle
        init = head r
        combining = tail r

main :: IO ()
main = do
  fileContents <- readFile "../data/triangle67.txt"
  let fileLines = lines fileContents
      fileParts = map words fileLines
      triangle = map (map (read :: String -> Integer)) fileParts
  print (maxPath triangle)