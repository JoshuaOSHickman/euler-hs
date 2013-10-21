import Data.Char
import Numeric

palindrome10 n = s == reverse s
  where s = show n

palindrome2 n = s == reverse s
  where s = showIntAtBase 2 intToDigit n ""

dualPalindromes = filter palindrome2 . filter palindrome10 $ [1..]

main = print . sum $ takeWhile (< 1000000) dualPalindromes