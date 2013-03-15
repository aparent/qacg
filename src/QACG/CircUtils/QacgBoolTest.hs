import CircUtils.QacgBool
import Test.QuickCheck

import Text.Printf
 
main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

exp1 = Xor $ map V ["a","b","c"]
exp2 = Xor [And (map V ["e","f","g"]), V "a"]
exp3 = And [exp1,exp2]
exp4 = Xor [exp3,exp1]
exp5 = And [exp3,exp4]

prop_simp exp a = if length a < 7 then True else evaluate a (simplify exp) == evaluate a exp

prop_flat exp a = if length a < 7 then True else evaluate a (flatten exp) == evaluate a exp

tests = [("Simplify/exp1", quickCheck (prop_simp exp1))
        ,("Simplify/exp2", quickCheck (prop_simp exp2))
        ,("Simplify/exp3", quickCheck (prop_simp exp3))
        ,("Simplify/exp4", quickCheck (prop_simp exp4))
        ,("Simplify/exp5", quickCheck (prop_simp exp5))
        ,("flat/exp1", quickCheck (prop_flat exp1))
        ,("flat/exp2", quickCheck (prop_flat exp2))
        ,("flat/exp3", quickCheck (prop_flat exp3))
        ,("flat/exp4", quickCheck (prop_flat exp4))
        ,("flat/exp5", quickCheck (prop_flat exp5))]
