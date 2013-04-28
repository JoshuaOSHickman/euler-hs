numbers :: [(Int, Int, Int, Int)]
numbers = [(read (show x ++ show y), read (show y ++ show z), x, z) |
	  	 x <- [0..9], y <- [0..9], z <- [0..9]]

good = filter (\ (a, b, c, d) -> fromIntegral a / fromIntegral b == fromIntegral c / fromIntegral d) numbers

lessone = filter (\ (a, b, c, d) -> c < d) good

nontrivial = filter (\ (a, _, _, __) -> a /= 0) lessone

numerators = map (\ (a, b, c, d) -> c) nontrivial
denominators = map (\ (a, b, c, d) -> d) nontrivial

numerator = product numerators
denominator = product denominators

factor = gcd numerator denominator

main = print (denominator `div` factor)


