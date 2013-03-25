import System.Environment

import QACG.CircUtils.Circuit
import QACG.CircUtils.CircuitToXML

import QACG.CircGen.Mult.SimpleMult
--import QACG.CircGen.Div.SimpleInt
import QACG.CircGen.Add.SimpleRipple
import QACG.CircGen.Bit.Shift
import QACG.CircGen.Bit.BitwiseOP
import QACG.CircGen.Bit.Equal
import QACG.CircGen.Comp.Ripple

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
    --"divUnsignedOutOfPlace"            ->  writeCircuit (dir++cName)  $ divUnsignedOutOfPlace size
    "greaterOutOfPlace"                ->  writeCircuit (dir++cName)  $ greaterOutOfPlace size
    "equalOutOfPlace"                  ->  writeCircuit (dir++cName)  $ equalOutOfPlace size
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
        greaterOutOfPlace n = mkRippleComp (var 'a' n) (var 'b' n) "z"
        equalOutOfPlace n = mkEqual (var 'a' n) (var 'b' n) "targ"
        -- divUnsignedOutOfPlace n =  genSimpleInt n --Fix this circuit up
        var vName n = [ vName:show x | x<-[0..n-1] ] 

writeCircuit :: String-> Circuit -> IO()
writeCircuit fname circ = 
  do writeFile (fname++".qc") (show circ)
     writeCircuitXML fname circ
     return() 
