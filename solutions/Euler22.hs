import Data.List(init, tail, sort)
import Data.List.Split(splitOn)
import Data.Char(ord, toLower, isAlpha)
import Data.Text(unpack, pack, strip)

charValue = subtract 96 . ord . toLower
nameValue = sum . map charValue

main = do
  filecontents <- readFile "../data/names22.txt"
  let quotedNames = splitOn "," filecontents
      names = map (filter isAlpha . unpack . strip . pack) quotedNames
      nameValues = map nameValue . sort $ names
      scores = zipWith (*) nameValues [1..]
  print $ sum scores