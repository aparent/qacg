import System.Environment
import System.Directory

import CircGen.Mult.SimpleMultAlt
import CircGen.Mult.Karatsubanew
import CircGen.Mult.SimpleMult
import CircGen.Mult.Wallace
import CircGen.Mult.PartialProduct
import CircGen.Add.SimpleRipple
import CircGen.Bit.Shift
import CircUtils.Circuit
import CircUtils.CircuitToXML
import Text.XML.HXT.Core
import CircGen.TestCircs.CS

main :: IO()
main = do 
  args   <- getArgs
  n      <- return $ read $ head args
  dir    <- return $ "examples-size-" ++ show n ++ "/"
  qcdir  <- return $ dir ++ "qc/"
  xmldir <- return $ dir ++ "xml/"
  createDirectoryIfMissing False dir
  createDirectoryIfMissing False qcdir
  createDirectoryIfMissing False xmldir
  writeCircuit qcdir xmldir "SimpleRipple"     $ simpleRipple n
  writeCircuit qcdir xmldir "SimpleRippleCtrl" $ simpleRippleCtrl n
  writeCircuit qcdir xmldir "SimpleMult"       $ simpleMult n
  writeCircuit qcdir xmldir "SimpleMultAlt"    $ simpleMultAlt n
  writeCircuit qcdir xmldir "wallace"          $ wallaceMult n
  writeCircuit qcdir xmldir "partialProduct"   $ partialProduct n
  writeCircuit qcdir xmldir "kara"             $ karatsuba n 8
  writeCircuit qcdir xmldir "ControlledShift"  $ contShift n 
  writeCircuit qcdir xmldir "CS"  $ cs

writeCircuit :: String-> String -> String -> Circuit -> IO()
writeCircuit qcdir xmldir fname circ = do 
  writeFile (qcdir ++ fname++".qc") (show circ)
  _ <- runX ( circToXML (xmldir++fname) circ )
  return() 
