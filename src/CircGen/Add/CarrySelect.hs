module CircGen.Add.CarrySelect
( genCarrySelect
) where

import Data.List
import CircUtils.Circuit
import CircGen.Add.SimpleRipple

genCarrySelect :: Int -> Int -> Circuit 
genCarrySelect a b = Circuit { line_info = adder_lines a (b-1), gates = adder_gates a (b-1), subcircuits= gen_adders a ++ gen_mult ((a)*2+1) }

applyCopy :: [String] -> [String] -> [Gates]
applyCopy [] _          = [] 
applyCopy (a:as) (b:bs) = Gate "tof" [a,b] : applyCopy as bs 

applyMux :: String -> [String] -> [String] -> [Gates]
applyMux _ [] _          = [] 
applyMux ctrl (a:as) (b:bs) = Gate "tof" [ctrl, a,b] : applyCopy as bs ctrl 

applyCSGroup :: String -> String -> [String] -> [String] -> ([Gate],[String])
applyCSGroup z c a b = (gates, c0 ++ c1 ++ z0 ++ z1
  where c0 = map (\x -> 'c':x) a
        c1 = map (\x -> 'c':x) b
				z0 = z ++ 'b'
				z1 = z ++ 'a'
				gates = combineLists (copy a ca) (copy b cb) ++ combineLists (applySimpleRipple a b z0 c0) (applySimpleRipple a b z1 c1)

combineLists :: [a] -> [a] -> [a]
combineLists [] [] = []
combineLists x  [] = x
combineLists []  y = y
combineLists (x:xs) (y:ys) = x : y : combineLists xs ys
