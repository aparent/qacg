module CircUtils.Circuit 
( Circuit(..)
  ,LineInfo(..)
  ,Gate(..)
  ,writeQc
  ,addLines
  ,addGates
  ,packCirc
) where

import Data.List

data Circuit = Circuit{   lineInfo :: LineInfo	      
			, gates :: [Gate]
			, subcircuits :: [(Circuit,String)]
}

data LineInfo = LineInfo{
  vars :: [String]
  , inputs :: [String]
  , outputs :: [String]
  , outputLabels :: [String]
}

data Gate = Gate{
    name :: String
  , lineNames :: [String]
}


instance Show Circuit where
  show = writeQc 

instance Show Gate where
  show = writeGate 

instance Show LineInfo where 
  show = writeLineInfo 

addLines :: [String] -> Circuit -> Circuit
addLines ls Circuit{lineInfo = l , gates=g, subcircuits=s} 
  = Circuit (LineInfo newLines (inputs l) (outputs l) (outputLabels l)  ) g s 
    where newLines  = union (vars l) ls 

addGates :: [Gate] -> Circuit -> Circuit
addGates gates Circuit{lineInfo = l , gates=g, subcircuits=s} 
  = Circuit l (g++gates) s 

inverse :: Circuit -> Circuit
inverse Circuit{lineInfo = l , gates=g, subcircuits=s} 
  = Circuit l (reverse g) s

packCirc :: Circuit -> Circuit
packCirc Circuit{lineInfo = l , gates=g, subcircuits=s} 
  = Circuit l (greedyPack g) s 

greedyPack :: [Gate] -> [Gate]
greedyPack gates = go gates 1
  where go gs n       | n < length gs = go (swapBack  gs n) (n + 1)
                      | otherwise     = gs 
        swapBack gs 0 = gs 
        swapBack gs 1 = gs 
        swapBack gs n | intersect (lineNames$gs!!n) (lineNames$gs!!(n-1)) == [] = swapBack ((take (n-1) gs)++[(gs!!n),(gs!!(n-1))] ++ drop (n+1) gs) (n-2)
                      | otherwise                                               = gs
        
-- |Takes a circuit and returns a string representing the .qc file
writeQc :: Circuit -> String
writeQc Circuit{lineInfo = l , gates=g, subcircuits=s} = writeLineInfo l++ writeSubcircuits s ++ "\nBEGIN\n" ++ writeGates g ++ "END\n"

writeSubcircuits :: [(Circuit,String)] -> String
writeSubcircuits = concatMap writeSubcircuit 

writeSubcircuit :: (Circuit,String) -> String
writeSubcircuit (c,n) =  "\nBEGIN "++ n ++"("++writeVars (vars $ lineInfo c) ++")\n" ++ writeGates (gates c) ++ "END " ++ n ++ "\n" 
                          ++ concatMap writeSubcircuit (subcircuits c)

writeLineInfo :: LineInfo -> String
writeLineInfo LineInfo{vars=v,inputs=i,outputs=o,outputLabels = ol} = 
	".v " ++ writeVars v ++ "\n.i "++ writeVars i ++ "\n.o " ++ writeVars o ++ "\n.ol " ++ writeVars ol ++ "\n"
	
writeVars :: [String] -> String
writeVars = intercalate " "

writeGates :: [Gate] -> String
writeGates = concatMap writeGate 

writeGate :: Gate -> String
writeGate Gate{name=n,lineNames=l} = n ++ " " ++ writeVars l ++ "\n"
