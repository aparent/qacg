import Text.XML.HXT.Core
import CircUtils.Circuit
import CircGen.Add.SimpleRipple
import CircGen.Mult.SimpleMultAlt
import Data.List

main :: IO ()
main
    = do
      --[src, dst] <- getArgs
      runX ( circToXML "ripple" (simpleRipple 4) )
      runX ( circToXML "mult" (simpleMultAlt 5) )
      return() 

circToXML :: String -> Circuit -> IOSArrow XmlTree XmlTree
circToXML pName circ = root [] [makeXMLProg pName circ]                    
            >>>
            writeDocument [withIndent yes] (pName ++ ".xml")

makeXMLProg  :: ArrowXml a => String -> Circuit -> a XmlTree XmlTree
makeXMLProg cname c = program $  (makeXMLCirc cname c) : map (\(c,n) -> makeXMLCirc n c) (subcircuits c) 
  where program a = mkelem "program" [ sattr "xmlns" "http://torque.bbn.com/ns/QuIGL",
                                         sattr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance", 
                                         sattr "xsi:schemaLocation" "http://torque.bbn.com/ns/QuIGL ../xsd/QuIGL.xsd"] 
                                         a 
     
makeXMLCirc  :: ArrowXml a => String -> Circuit -> a XmlTree XmlTree
makeXMLCirc procName Circuit{lineInfo = l , gates=g, subcircuits=s}
    = proceedure $ map mkGate g 
    where proceedure a =  mkelem "procedure" [ sattr "name" procName,
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
          
