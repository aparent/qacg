
import qualified Data.Map as Map
import qualified Data.List as List

import CircUtils.Circuit
import CircUtils.QacgBool 

import CircGen.Mult.PartialProduct
import CircGen.Mult.Wallace
import CircGen.Add.Ripple
import CircGen.Add.SimpleRipple

type SCirc = ([SLine],[SGate])

type SLine = (String,BExpr)
type SGate = [String]

data SGType = TOF | SWAP

applyGates :: SCirc -> [SLine]
applyGates (lines,[])   = lines
applyGates (lines,gate:gates) = map (\(x,y) -> (x,flatten y))  $ applyGates (applyG lMap gate, gates)
  where lMap = Map.fromList lines
        applyG l g | length g == 1 =  Map.toList $ Map.adjust ( \l -> Xor [C True,l] ) (head g) l  -- Not
                   | length g == 2 =  Map.toList $ Map.adjust ( \l -> Xor [head (gExp $ tail g),l] ) (head g) l -- Cnot
                   | otherwise = Map.toList $ Map.adjust ( applyTof $ gExp $ tail g) (head g) l --Toff
          where applyTof g l =   Xor [ And g, l]		
                gExp g = snd $ unzip $ Map.toList $ Map.filterWithKey (\x _-> x `elem` g) lMap  

simQCirc :: Circuit -> [SLine]
simQCirc c = applyGates(convToSCirc c)

convToSCirc:: Circuit -> SCirc
convToSCirc Circuit{line_info = l , gates=g, subcircuits=s} = (getLines (vars l), getGates g) 

getLines :: [String] -> [SLine]  
getLines = map (\ x -> (x, V x))

getGates :: [Gate] -> [SGate]
getGates = map (List.reverse $line_names) 
