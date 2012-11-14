sumSquares lst = sum (map (\x -> x * x) lst)
squareSum lst = s * s where s = sum lst

value = squareSum n - sumSquares n where n = [1..100]

main = print value

