value = sum (filter (\x -> 0 == rem x 3 || 0 == rem x 5) [1..999])

main = print value
