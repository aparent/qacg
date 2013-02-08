module CircGen.Mult.SimpleMultAlt
( simpleMultAlt
	,applySimpleMultAlt
) where

import CircUtils.Circuit
import CircGen.Add.SimpleRipple

-- | Generates a simple addition based multiplication circuit 
simpleMultAlt :: Int -> Circuit 
simpleMultAlt n = Circuit (multLines n) (multGates n) [] --[(simpleRippleCtrl n, "CADD")]

applySimpleMultAlt :: [String] -> [String] -> [String] -> String -> [Gate]
applySimpleMultAlt a b r c = start ++ go (tail a) (tail r) 
  where start = zipWith (\x y -> Gate "tof" [(head a),x,y]) b r
        go [] _ = [] 
        go (x:xs) res = (applySimpleRippleCtrl x b (take (length b) res) (res !! (length b)) c) ++ (go xs $ tail res)

multGates :: Int -> [Gate]
multGates n = applySimpleMultAlt a b res "c"
  where (a,b,res) = lNames n

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

--Uncomment this and part in simple mult alt to use the version with subcircuits
--multGates :: Int -> [Gate]
--multGates n = start ++ adders (tail a) b (tail res) n
--  where start = map (\x -> Gate "tof" [(head a),(b!!x),(res!!x)]) [0..(n-1)]
--        (a,b,res) = lNames n

--adders :: [String] -> [String] -> [String] -> Int -> [Gate]
--adders a b res n | length res > n = (Gate "CADD" $ head a : "c" : (combLists b $ take n res) ++ [res !! n]):(adders (tail a) b (tail res) n)
--                 | otherwise = []

--combLists :: [x] -> [x] -> [x]
--combLists a b = concat $ zipWith (\x y -> [x,y]) a b
