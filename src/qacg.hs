import System.Environment
import System.Directory

import CircGen.Mult.SimpleMultAlt
import CircGen.Mult.SimpleMult
import CircGen.Mult.Wallace
import CircGen.Mult.PartialProduct
import CircGen.Add.SimpleRipple
import CircUtils.Circuit

main :: IO()
main = do 
  args <- getArgs
  n    <- return $ read $ head args
  dir  <- return $ "ExamplesSize-" ++ show n ++ "/"
  createDirectoryIfMissing False dir
  writeCircuit dir "SimpleRipple.qc"  $ simpleRipple n
  writeCircuit dir "SimpleRippleCtrl.qc"  $ simpleRippleCtrl n
  writeCircuit dir "SimpleMult.qc"  $ simpleMult n
  writeCircuit dir "SimpleMultAlt.qc" $ simpleMultAlt n
  writeCircuit dir "wallace.qc" $ wallaceMult n
  writeCircuit dir "partialProduct.qc" $ partialProduct n

writeCircuit :: String -> String -> Circuit -> IO()
writeCircuit dir fname circ = writeFile (dir ++ fname) (show circ)
