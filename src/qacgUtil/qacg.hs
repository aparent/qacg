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
    -- Arithmetic Ops
    "adderUnsignedInPlaceCarry"        ->  writeCircuit (dir++cName) $ simpleRippleSize size
    "adderUnsignedInPlaceCarryControl" ->  writeCircuit (dir++cName) $ simpleCtrlRippleSize size
    "adderUnsignedInPlaceModulus"      ->  notImplemented cName
    "adderUnsignedOutOfPlaceModulus"   ->  notImplemented cName
    "subtractUnsignedInPlace"          ->  writeCircuit (dir++cName) $ subtractUnsignedInPlace size --not in list fix this
    "subtractUnsignedInPlaceModulus"   ->  notImplemented cName
    "multUnsignedOutOfPlace"           ->  writeCircuit (dir++cName) $ simpleMultSize size
    "multUnsignedOutOfPlaceModulus"    ->  notImplemented cName
    "divUnsignedOutOfPlace"            ->  notImplemented cName
    "modUnsignedOutOfPlace"            ->  notImplemented cName
    -- Bit Ops
    "shiftLeftControl"                 ->  writeCircuit (dir++cName) $ contShift size
    "shiftRightControl"                ->  notImplemented cName
    "notInPlace"                       ->  notImplemented cName
    "andOutOfPlace"                    ->  writeCircuit (dir++cName) $ andOutOfPlace size
    "orOutOfPlace"                     ->  writeCircuit (dir++cName) $ orOutOfPlace size
    "xorOutOfPlace"                    ->  writeCircuit (dir++cName) $ xorOutOfPlace size
    "xorInPlace"                       ->  notImplemented cName
    "nandOutOfPlace"                   ->  writeCircuit (dir++cName) $ nandOutOfPlace size
    "norOutOfPlace"                    ->  writeCircuit (dir++cName) $ norOutOfPlace size
    -- Relational Ops
    "equalOutOfPlace"                  ->  writeCircuit (dir++cName) $ equalOutOfPlace size
    "notEqualOutOfPlace"               ->  writeCircuit (dir++cName) $ notEqualOutOfPlace size
    "lessOutOfPlace"                   ->  writeCircuit (dir++cName) $ lessOutOfPlace size
    "greaterOutOfPlace"                ->  writeCircuit (dir++cName) $ greaterOutOfPlace size
    "lessThanOrEqualOutOfPlace"        ->  writeCircuit (dir++cName) $ lessThanOrEqualOutOfPlace size
    "greaterThenOrEqualOutOfPlace"     ->  writeCircuit (dir++cName) $ greaterThenOrEqualOutOfPlace size
    _ -> putStrLn $ "No generator for " ++ cName
  where simpleRippleSize n              = mkSimpleRipple (var 'a' n) (var 'b' n) "z"
        subtractUnsignedInPlace n       = mkSimpleSubtract (var 'a' n) (var 'b' n)
        simpleCtrlRippleSize n          = mkSimpleCtrlRipple "control" (var 'a' n) (var 'b' n) "z"
        simpleMultSize n                = mkSimpleMult  (var 'a' n) (var 'b' n)
        andOutOfPlace n                 = mkBitwiseAND  (var 'a' n) (var 'b' n) (var 'z' n)
        nandOutOfPlace n                = mkBitwiseNAND (var 'a' n) (var 'b' n) (var 'z' n)
        orOutOfPlace  n                 = mkBitwiseOR   (var 'a' n) (var 'b' n) (var 'z' n)
        norOutOfPlace  n                = mkBitwiseNOR   (var 'a' n) (var 'b' n) (var 'z' n)
        xorOutOfPlace n                 = mkBitwiseXOR  (var 'a' n) (var 'b' n) (var 'z' n)
        lessOutOfPlace n                = mkLessOutOfPlace  (var 'a' n) (var 'b' n) "z"
        greaterOutOfPlace n             = mkGreaterOutOfPlace  (var 'a' n) (var 'b' n) "z"
        lessThanOrEqualOutOfPlace n     = mkLessThanOrEqualOutOfPlace  (var 'a' n) (var 'b' n) "z"
        greaterThenOrEqualOutOfPlace n  = mkGreaterThenOrEqualOutOfPlace  (var 'a' n) (var 'b' n) "z"
        equalOutOfPlace n               = mkEqualOutOfPlace (var 'a' n) (var 'b' n) "targ"
        notEqualOutOfPlace n            = mkNotEqualOutOfPlace (var 'a' n) (var 'b' n) "targ"
        -- divUnsignedOutOfPlace n =  genSimpleInt n --Fix this circuit up
        var vName n = [ vName:show x | x<-[0..n-1] ] 
        notImplemented nm = putStrLn $ nm ++ " is not yet implemented."

writeCircuit :: String-> Circuit -> IO()
writeCircuit fname circ = 
  do writeFile (fname++".qc") (show circ)
     writeCircuitXML fname circ
     return() 
