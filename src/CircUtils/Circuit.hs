module CircUtils.Circuit 
( Circuit(..)
  ,LineInfo(..)
  ,Gate(..)
  ,writeQc
) where

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

-- |Takes a circuit and returns a string representing the .qc file
writeQc :: Circuit -> String
writeQc Circuit{lineInfo = l , gates=g, subcircuits=s} = writeLineInfo l++ writeSubcircuits s ++ "\nBEGIN\n" ++ writeGates g ++ "END\n"

writeSubcircuits :: [(Circuit,String)] -> String
writeSubcircuits = concatMap writeSubcircuit 

writeSubcircuit :: (Circuit,String) -> String
writeSubcircuit (c,n) =  "\nBEGIN "++ n ++"( "++writeVars (vars $ lineInfo c) ++")\n" ++ writeGates (gates c) ++ "END " ++ n ++ "\n" 
                          ++ concatMap writeSubcircuit (subcircuits c)

writeLineInfo :: LineInfo -> String
writeLineInfo LineInfo{vars=v,inputs=i,outputs=o,outputLabels = ol} = 
	".v " ++ writeVars v ++ "\n.i "++ writeVars i ++ "\n.o " ++ writeVars o ++ "\n.ol " ++ writeVars ol ++ "\n"
	
writeVars :: [String] -> String
writeVars = concatMap (\x -> ' ':x) 

writeGates :: [Gate] -> String
writeGates = concatMap writeGate 

writeGate :: Gate -> String
writeGate Gate{name=n,lineNames=l} = n ++ " " ++ writeVars l ++ "\n"
