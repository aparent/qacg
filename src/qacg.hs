import System.Environment

import CircGen.Mult.SimpleMultAlt
import CircGen.Add.SimpleRipple
import CircUtils.Circuit
import CircUtils.CircuitToXML
import Text.XML.HXT.Core

main :: IO()
main = do 
  args   <- getArgs
  cName  <- return $ head args
  size   <- return $ read $ head $ tail args
  case cName of
    "adder"     ->  writeCircuit "SimpleRipple" $ simpleRipple size
    "adderCtrl" ->  writeCircuit "SimpleRippleCtrl"  $ simpleRippleCtrl size
    "multiplier" -> writeCircuit "SimpleMult" $ simpleMultAlt size
    _ -> putStrLn $ "No generator for " ++ cName

writeCircuit :: String-> Circuit -> IO()
writeCircuit fname circ = do 
  writeFile (fname++".qc") (show circ)
  _ <- runX ( circToXML fname circ )
  return() 
