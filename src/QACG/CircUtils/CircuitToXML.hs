module QACG.CircUtils.CircuitToXML( 
  writeCircuitXML 
) where

import Text.XML.HXT.Core
import QACG.CircUtils.Circuit
--import Data.List

-- | takes a filname and a circuit and writes the circuit in XML format 
writeCircuitXML :: String -> Circuit -> IO()
writeCircuitXML fname circ  = 
  do _ <- runX ( circToXML fname circ )
     return() 

circToXML :: String -> Circuit -> IOSArrow XmlTree XmlTree
circToXML pName circ = root [] [makeXMLProg pName circ]                    
            >>>
            writeDocument [withIndent yes] (pName ++ ".xml")

makeXMLProg  :: ArrowXml a => String -> Circuit -> a XmlTree XmlTree
makeXMLProg cname c = program $ statement $ makeXMLCirc cname c : map (\(x,y) -> makeXMLCirc y x) (subcircuits c) 
  where program = mkelem "circuit" [ 
                    sattr "xmlns" "http://torque.bbn.com/ns/QuIGL",
                    sattr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance", 
                    sattr "xsi:schemaLocation" "http://torque.bbn.com/ns/QuIGL ../xsd/QuIGL.xsd"
                 ] 
        statement a = [ selem "statement" a ]
     
makeXMLCirc  :: ArrowXml a => String -> Circuit -> a XmlTree XmlTree
makeXMLCirc procName circ = proceedure $ map mkGate $ gates circ
  where qVars = map mkVar $ vars $ lineInfo circ  
        proceedure a =  selem "procedure" [ 
                           selem "name" [txt procName]
                          ,selem "quantum" qVars
                          ,selem "annotations" $ map mkAnnotation $ circuitAnnotations circ
                          ,selem "block" a 
                        ]
        mkAnnotation (l,q) = selem "annotation" [ 
                                selem "label" [txt l]
                               ,selem "quantity" [ 
                                 selem "integer" [txt q]
                               ]
                             ]
        mkVar v = selem "variable" [selem "name" [(txt v)]]
        mkGate g = selem "statement" [
                     selem "gate" [ 
                        selem  "name" [txt $name g]
                       ,selem "quantum" (map (\x-> selem "qurange" [mkVar x]) $ lineNames g) 
                     ]
                   ]

