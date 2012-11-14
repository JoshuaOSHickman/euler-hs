import Data.Set(Set)
import qualified Data.Set as Set

possibilities a b = p' a b Set.empty
  where 
    p' 1 b s = s
    p' a 1 s = p' (a - 1) b s -- I did a little ditty with smart shadowing there
    p' a b s = p' a (b - 1) (Set.insert (a ^ b) s)

main = print $ Set.size (possibilities 100 100)