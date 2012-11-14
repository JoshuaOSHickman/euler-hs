limit = 1000000

digits n = if n == 0
           then []
           else (n `rem` 10) : digits (n `div` 10)

isSumOfFifthPowerDigits n = n == sum (map (^ 5) (digits n))

main = print $ sum $ filter isSumOfFifthPowerDigits [2..limit]