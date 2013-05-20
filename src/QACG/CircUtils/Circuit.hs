module QACG.CircUtils.Circuit 
( Circuit(..)
  ,LineInfo(..)
  ,Gate(..)
  ,writeQc
  ,addLines
  ,addGates
  ,circuitAnnotations
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
    where newLines  = vars l `union` ls 

addGates :: [Gate] -> Circuit -> Circuit
addGates newGates Circuit{lineInfo = l , gates=g, subcircuits=s} 
  = Circuit l (g++newGates) s 

-- |Takes a circuit and returns a string representing the .qc file
writeQc :: Circuit -> String
writeQc Circuit{lineInfo = l , gates=g, subcircuits=s} = writeLineInfo l++ writeSubcircuits s ++ "\nBEGIN\n" ++ writeGates g ++ "END\n"

writeSubcircuits :: [(Circuit,String)] -> String
writeSubcircuits = concatMap writeSubcircuit 

writeSubcircuit :: (Circuit,String) -> String
writeSubcircuit (c,n) =  "\nBEGIN "++ n ++"("++ unwords (vars $ lineInfo c) ++")\n" ++ writeGates (gates c) ++ "END " ++ n ++ "\n" 
                          ++ concatMap writeSubcircuit (subcircuits c)

writeLineInfo :: LineInfo -> String
writeLineInfo LineInfo{vars=v,inputs=i,outputs=o,outputLabels = ol} = 
	".v " ++ unwords v ++ "\n.i "++ unwords i ++ "\n.o " ++ unwords o ++ "\n.ol " ++ unwords ol ++ "\n"
	
writeGates :: [Gate] -> String
writeGates = concatMap writeGate 

writeGate :: Gate -> String
writeGate Gate{name=n,lineNames=l} = n ++ " " ++ unwords l ++ "\n"

numGate :: Circuit -> String -> Int
numGate c s = numGate' $ gates c
  where numGate' (g:gs) = if name g == s 
                            then 1 + numGate' gs
                            else 0 + numGate' gs
        numGate' [] = 0 

circuitAnnotations :: Circuit -> [(String,String)]
circuitAnnotations c = [("countT",show numT)
                       ,("countCNOT",show numCNOT)
                       ,("countH", show numH)
                       ,("width_max",show widthMax)
                       ,("width_delta",show widthDelta)]--("depthT","0")
  where numT = numGate c "T" + numGate c "T*"
        numCNOT = numGate c "TOF"
        numH = numGate c "H"
        widthMax = length $ vars $ lineInfo $ c
        widthDelta = widthMax - (length$inputs$lineInfo$c)
