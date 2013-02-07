import Text.XML.HXT.Core
import CircUtils.Circuit
import CircGen.Add.SimpleRipple
import Data.List

main :: IO ()
main
    = do
      --[src, dst] <- getArgs
      runX ( circToXML "ripple" (simpleRipple 5) )
      return() 

circToXML :: String -> Circuit -> IOSArrow XmlTree XmlTree
circToXML pName circ = root [] [makeXMLCirc pName circ]                    
            >>>
            writeDocument [withIndent yes] (pName ++ ".xml")

makeXMLCirc  :: ArrowXml a => String -> Circuit -> a XmlTree XmlTree
makeXMLCirc procName Circuit{lineInfo = l , gates=g, subcircuits=s}
    = program $ [ proceedure $ map mkGate g ]
    where program a = mkelem "program" [ sattr "xmlns" "http://torque.bbn.com/ns/QuIGL",
                                         sattr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance", 
                                         sattr "xsi:schemaLocation" "http://torque.bbn.com/ns/QuIGL ../xsd/QuIGL.xsd"] 
                                         a 
          proceedure a =  mkelem "procedure" [ sattr "name" procName,
                                         sattr "static" "", 
                                         sattr "quantum" lines] 
                                         [mkelem "body" [] a ]
          mkGate Gate{name = gname, lineNames = lnames} = mkelem "control" [] $ (map (\x -> mkelem "control"[][ mkelem "variable" [][ txt x ]]) (tail $ reverse lnames)) ++ [
                                                            mkelem "body" [][
                                                              mkelem "statement" [][
                                                                mkelem "operator"[][txt gname], 
                                                                mkelem "arguments"[][ 
                                                                  mkelem "variable" [][
                                                                    txt (head$reverse lnames)
                                                         ]]]]]
          lines = concat $ intersperse "," $ vars l  
          
