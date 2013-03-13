import System.Environment

import CircUtils.Circuit
import CircUtils.CircuitToXML
import Text.XML.HXT.Core

import CircGen.Mult.SimpleMult
import CircGen.Add.SimpleRipple
import CircGen.Bit.Shift
import CircGen.Bit.BitwiseOP

--usage: qacg <directory> <circuitName> <size>
main :: IO()
main = do 
  args  <- getArgs
  let cName = head args
  let size  = read $ head $ tail args
  let dir   = head $ tail $ tail args
  case cName of
    "adderUnsignedInPlaceCarry"        ->  writeCircuit (dir++cName)  $ simpleRippleSize size
    "adderUnsignedInPlaceCarryControl" ->  writeCircuit (dir++cName)  $ simpleCtrlRippleSize size
    "multUnsignedOutOfPlace"           ->  writeCircuit (dir++cName)  $ simpleMultSize size
    "shiftLeftControl"                 ->  writeCircuit (dir++cName)  $ contShift size
    "andOutOfPlace"                    ->  writeCircuit (dir++cName)  $ andOutOfPlace size
    "orOutOfPlace"                     ->  writeCircuit (dir++cName)  $ orOutOfPlace size
    "xorOutOfPlace"                    ->  writeCircuit (dir++cName)  $ xorOutOfPlace size
    _ -> putStrLn $ "No generator for " ++ cName
  where simpleRippleSize n = mkSimpleRipple (var 'a' n) (var 'b' n) "z"
        simpleCtrlRippleSize n = mkSimpleCtrlRipple "control" (var 'a' n) (var 'b' n) "z"
        simpleMultSize n = mkSimpleMult (var 'a' n) (var 'b' n)
        andOutOfPlace n =  mkBitwiseAND (var 'a' n) (var 'b' n) (var 'z' n)
        orOutOfPlace  n =  mkBitwiseOR  (var 'a' n) (var 'b' n) (var 'z' n)
        xorOutOfPlace n =  mkBitwiseXOR (var 'a' n) (var 'b' n) (var 'z' n)
        var vName n = [ vName:show x | x<-[0..n-1] ] 

writeCircuit :: String-> Circuit -> IO()
writeCircuit fname circ = do 
  writeFile (fname++".qc") (show circ)
  _ <- runX ( circToXML fname circ )
  return() 
