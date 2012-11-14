digitLength 0 = 0 -- remember to add in "zero" if needed
digitLength 1 = 3 -- one
digitLength 2 = 3 -- two
digitLength 3 = 5 -- three
digitLength 4 = 4 -- four
digitLength 5 = 4 -- five
digitLength 6 = 3 -- six
digitLength 7 = 5 -- seven
digitLength 8 = 5 -- eight
digitLength 9 = 4 -- nine

twoDigitLength' 10 = 3 -- ten
twoDigitLength' 11 = 6 -- eleven
twoDigitLength' 12 = 6 -- twelve
twoDigitLength' 13 = 8 -- thirteen
twoDigitLength' 14 = 8 -- fourteen
twoDigitLength' 15 = 7 -- fifteen
twoDigitLength' 16 = 7 -- sixteen
twoDigitLength' 17 = 9 -- seventeen
twoDigitLength' 18 = 8 -- eighteen
twoDigitLength' 19 = 8 -- nineteen

tens 2 = 6 -- twenty
tens 3 = 6 -- thirty
tens 4 = 5 -- forty
tens 5 = 5 -- fifty
tens 6 = 5 -- sixty
tens 7 = 7 -- seventy
tens 8 = 6 -- eighty
tens 9 = 6 -- ninety

twoDigitLength n 
  | n < 10 = digitLength n
  | n < 20 = twoDigitLength' n
  | otherwise = let (d, m) = n `divMod` 10
                in tens d + digitLength m

threeDigitLength n = let (d, m) = n `divMod` 100
                         base = 7 -- hundred
                         additional = 3 -- and
                         firstpart = if d == 0
                                     then 0
                                     else digitLength d + base
                     in if m == 0
                        then firstpart
                        else if d == 0
                             then twoDigitLength m
                             else firstpart + additional + twoDigitLength m

main = print $ sum (map threeDigitLength [1..999]) + 11 -- one thousand
