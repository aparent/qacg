module CircGen.Mult.PartialProduct
( partialProduct
) where

import Data.List
import CircUtils.Circuit

partialProduct :: Int -> Circuit
partialProduct a = Circuit { lineInfo = partialLines a, gates = partialGates a, subcircuits= []}

partialLines :: Int -> LineInfo
partialLines a = 
	LineInfo { vars = partialInputs a ++ partialAncilla a
                  , inputs = partialInputs a  
                  , outputs = partialInputs a ++ partialAncilla a
                  , outputLabels = partialInputs a ++ partialAncilla a  }

partialInputs ::  Int -> [String]
partialInputs a = zipWith (++) (take (a*2) (cycle ["a","b"])) (concatMap (replicate 2) (map show [0..a])) 

partialAncilla :: Int -> [String]
partialAncilla a =  let blist = take a (zipWith (++) (repeat "b") $ map show [0::Int ..])
                        alist = take a (zipWith (++) (repeat "a") $ map show [0::Int ..])
		in  map (uncurry(++)) $ sortBy partialLineSort $ concatMap (\x -> map (\y -> (x,y)) blist) alist --sort so they are grouped better for adders 

partialGates :: Int -> [Gate]
partialGates a = let blist = take a (zipWith (++) (repeat "b") $ map show [0::Int ..])
                     alist = take a (zipWith (++) (repeat "a") $ map show [0::Int ..])
		in map (Gate "TOF" ) (concatMap (\x-> map (\y-> [x,y,x++y]) blist) alist)

partialLineSort :: (String,String) -> (String,String) -> Ordering
partialLineSort  (_:x1,_:y1) (_:x2, _:y2)  = compare (read x1+read y1) (read x2+read y2)
