 -- the key thing to note here is that the lexicographic position can be
 -- determined by rotations as if in base factorial. 

 -- I am aware that's cryptic, but hopefully the code will explain

 -- the general idea for a small case
 -- 012   021   102   120   201   210 -- permutation
 --  0     1     2     3     4     5  -- index
 --  0     1     10    11    20    21 -- base factorial of index

 -- more clearly

 -- 0123 0132 0213 0231 0312 0321 1023 1032 1203 1230 ...
 --   0    1    2    3    4    5    6    7    8    9
 -- 000  001  010  011  020  021  100  101  110  111

basePermutation = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
offsets n size = padding ++ digits ++ [0]
  where digits = baseFactorial n
        padding = replicate (size - length digits - 1) 0

without i xs = take i xs ++ drop (i + 1) xs

shift digits [] = digits 
shift digits (offset:offsets) = (digits !! offset) : shift (without offset digits) offsets

factorial n = product [1..n]

baseFactorial 0 = []
baseFactorial n = digit : (continued ++ otherDigits)
  where (i, base) = last . takeWhile (\(a, b) -> b <= n) . map (\n -> (n - 1, factorial n)) $ [1..]
        (digit, carry) = n `divMod` base
        continued = baseFactorial carry
        otherDigits = replicate (i -  length (baseFactorial carry)) 0

nthPermutation n digits = shift digits (offsets n (length digits))

toNumber :: [Integer] -> Integer
toNumber = read . concatMap show

main = print . toNumber $ nthPermutation 999999 basePermutation
