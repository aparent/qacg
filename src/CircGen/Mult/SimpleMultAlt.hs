module CircGen.Mult.SimpleMultAlt
( simpleMultAlt
) where


import Data.List
import CircUtils.Circuit

import CircGen.Add.SimpleRipple

-- | Generates a simple addition based multiplication circuit 
simpleMultAlt :: Int -> Circuit 
simpleMultAlt n = Circuit (multLines n) (multGates n) [(simpleRippleCtrl n, "CADD")]

multLines :: Int -> LineInfo 
multLines n = LineInfo vars inputs outputs outputLab 
  where vars = a ++ b ++["c"] ++ res
        inputs = a ++ b
        outputs = vars 
        outputLab = vars  
        (a,b,res) = lNames n

lNames :: Int -> ([String],[String],[String])
lNames n = (a,b,res)
  where a = map (\x -> 'a' : show x) [0..(n-1)]  
        b = map (\x -> 'b' : show x) [0..(n-1)]  
        res = map (\x -> 'r' : show x) [0..(n*2-1)]  

multGates :: Int -> [Gate]
multGates n = start ++ adders (tail a) b (tail res) n
  where start = map (\x -> Gate "tof" [(head a),(b!!x),(res!!x)]) [0..(n-1)]
        (a,b,res) = lNames n

adders :: [String] -> [String] -> [String] -> Int -> [Gate]
adders a b res n | length res > n = (Gate "CADD" $ head a : "c" : (combLists b $ take n res) ++ [res !! n]):(adders (tail a) b (tail res) n)
                 | otherwise = []

combLists :: [x] -> [x] -> [x]
combLists a b = concat $ zipWith (\x y -> [x,y]) a b
