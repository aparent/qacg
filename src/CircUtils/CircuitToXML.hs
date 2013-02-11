
module CircUtils.CircuitToXML( 
  circToXML 
) where

import Text.XML.HXT.Core
import CircUtils.Circuit
import Data.List

circToXML :: String -> Circuit -> IOSArrow XmlTree XmlTree
circToXML pName circ = root [] [makeXMLProg pName circ]                    
            >>>
            writeDocument [withIndent yes] (pName ++ ".xml")

makeXMLProg  :: ArrowXml a => String -> Circuit -> a XmlTree XmlTree
makeXMLProg cname c = program $ makeXMLCirc cname c : map (\(x,y) -> makeXMLCirc y x) (subcircuits c) 
  where program = mkelem "program" [ 
                    sattr "xmlns" "http://torque.bbn.com/ns/QuIGL",
                    sattr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance", 
                    sattr "xsi:schemaLocation" "http://torque.bbn.com/ns/QuIGL ../xsd/QuIGL.xsd"
                 ] 
     
makeXMLCirc  :: ArrowXml a => String -> Circuit -> a XmlTree XmlTree
makeXMLCirc procName circ = proceedure $ map mkGate $ gates circ
    where qVars = intercalate "," $ vars $ lineInfo circ  
          proceedure a =  mkelem "procedure" [ 
                              sattr "name" procName,
                              sattr "static" "", 
                              sattr "quantum" qVars
                            ] 
                            [ mkelem "body" [] a ]
          mkGate g = mkelem "control" [] $ controls ++ [body]
            where controls = map mkControl (tail$reverse$lineNames g)
                  body = selem "body" [
                           selem "statement" [
                             selem "operator" [ 
                               txt$name g
                             ], 
                             selem "arguments" [ 
                               selem "variable" [
                                 txt (head$reverse$lineNames g)
                               ]
                             ]
                           ]
                         ]
                  mkControl x = selem "control" [ 
                                  selem "variable" [ 
                                    txt x 
                                  ]
                                ] 
