nextPrimeFactor n minimum = if 0 == rem n minimum
                            then minimum
                            else nextPrimeFactor n (1 + minimum)

withoutFactor n f = if 0 == rem n f
                    then withoutFactor (div n f) f
                    else n

primeFactors n minimum = if n == 1
                         then []
                         else factor : primeFactors (withoutFactor n factor) (1 + factor)
  where factor = nextPrimeFactor n minimum

main = print (maximum (primeFactors 600851475143 2))
