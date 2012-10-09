module CircGen.Mult.SimpleMult
( simpleMult
) where


import Data.List
import CircUtils.Circuit

import CircGen.Add.SimpleRipple

-- | Generates a simple addition based multiplication circuit 
simpleMult :: Int -> Circuit 
simpleMult n = Circuit (multLines n) (multGates n) [(simpleRipple n, "ADD")]

multLines :: Int -> LineInfo 
multLines n = LineInfo vars inputs outputs outputLab 
  where vars = a ++ b ++ pp ++ ["c"] ++ res
        inputs = a ++ b
        outputs = vars 
        outputLab = vars  
        (a,b,pp,res) = lNames n

lNames :: Int -> ([String],[String],[String],[String])
lNames n = (a,b,pp,res)
  where a = map (\x -> 'a' : show x) [1..n]  
        b = map (\x -> 'b' : show x) [1..n]  
        pp = map (\x -> 'p' : show x) [1..n]  
        res = map (\x -> 'r' : show x) [1..(n*2)]  

multGates :: Int -> [Gate]
multGates n = concat $ combinePP (partialGates a b pp) (adders pp res)
  where combinePP pp add = concat $ zipWith (\x y -> [x,y,reverse x]) pp add
        (a,b,pp,res) = lNames n

adders :: [String] -> [String] -> [[Gate]]
adders pp res | length pp < length res = [Gate "ADD" adderIn] : adders pp (tail res)
              | otherwise = []
  where combLists :: [x] -> [x] -> [x]
        combLists a b = concat $ zipWith (\x y -> [x,y]) a b
        adderIn = "c" : combLists pp (init adderRes) ++ [last adderRes]
        adderRes = take (length pp + 1) res
       

partialGates :: [String] -> [String] -> [String] -> [[Gate]]
partialGates a b c = map (zipWith (\z (x,y) -> Gate "tof" [x,y,z]) c) pairGroups    
   where pairGroups = map (pairSet b) a
         pairSet l s = map (\x -> (s,x)) l 

combLists :: [x] -> [x] -> [x]
combLists a b = concat $ zipWith (\x y -> [x,y]) a b
