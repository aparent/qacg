import System.Environment

import CircGen.Mult.SimpleMultAlt
import CircGen.Add.SimpleRipple
import CircGen.Bit.Shift
import CircUtils.Circuit
import CircUtils.CircuitToXML
import Text.XML.HXT.Core

--usage: qacg <directory> <circuitName> <size>
main :: IO()
main = do 
  args  <- getArgs
  cName <- return $ head args
  size  <- return $ read $ head $ tail args
  dir   <- return $ head $ tail $ tail args
  case cName of
    "adder"      ->  writeCircuit (dir++"adder"     )  $ simpleRipple size
    "adderCtrl"  ->  writeCircuit (dir++"adderCtrl" )  $ simpleRippleCtrl size
    "multiplier" ->  writeCircuit (dir++"multiplier")  $ simpleMultAlt size
    "shiftCtrl"  ->  writeCircuit (dir++"shiftCtrl" )  $ contShift size
    _ -> putStrLn $ "No generator for " ++ cName

writeCircuit :: String-> Circuit -> IO()
writeCircuit fname circ = do 
  writeFile (fname++".qc") (show circ)
  _ <- runX ( circToXML fname circ )
  return() 
