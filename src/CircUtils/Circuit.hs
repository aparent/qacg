module CircUtils.Circuit 
( Circuit(..)
  ,LineInfo(..)
  ,Gate(..)
  ,writeQc
  ,addLines
  ,addGates
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
addGates newGates Circuit{lineInfo = l , gates=g, subcircuits=s} 
  = Circuit l (g++newGates) s 

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
