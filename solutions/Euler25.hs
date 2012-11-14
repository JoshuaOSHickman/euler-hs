phi = 1.6180339887

 -- 1000 digits means 
 -- n = logBase phi (10 ** 1000)
 -- but logBase in haskell explodes with those numbers

 -- so we really want 1000 * logBase phi 10
 -- we take the ceiling because we need the number _over_ 10 ** 1000 to get all the digits we need
 -- we add 1 because of the indices of the starting terms
 -- except 10 ** 1 has two digits, so we only need 10 ** 999, so it's a * 999 after the reduction
 -- out of the log

phiEstimate = (+ 1) . ceiling . (* 999) $ logBase phi 10 

main = print phiEstimate

