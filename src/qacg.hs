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
  cName <- return $ head args
  size  <- return $ read $ head $ tail args
  dir   <- return $ head $ tail $ tail args
  case cName of
    "adderUnsignedInPlaceCarry"        ->  writeCircuit (dir++cName)  $ simpleRippleSize size
    "adderUnsignedInPlaceCarryControl" ->  writeCircuit (dir++cName)  $ simpleCtrlRippleSize size
    "multUnsignedOutOfPlace"           ->  writeCircuit (dir++cName)  $ simpleMultSize size
    "shiftLeftControl"                 ->  writeCircuit (dir++cName)  $ contShift size
    "andOutOfPlace"                    ->  writeCircuit (dir++cName)  $ andOutOfPlace size
    "orOutOfPlace"                     ->  writeCircuit (dir++cName)  $ orOutOfPlace size
    "xorOutOfPlace"                    ->  writeCircuit (dir++cName)  $ xorOutOfPlace size
    _ -> putStrLn $ "No generator for " ++ cName
  where simpleRippleSize n = mkSimpleRipple ['a':show x|x<-[0..n-1]] ['b':show x|x<-[0..n-1]] "z"
        simpleCtrlRippleSize n = mkSimpleCtrlRipple "control" ['a':show x|x<-[0..n-1]] ['b':show x|x<-[0..n-1]] "z"
        simpleMultSize n = mkSimpleMult ['a':show x|x<-[0..n-1]] ['b':show x|x<-[0..n-1]]
        andOutOfPlace n = mkBitwiseAND ['a':show x|x<-[0..n-1]] ['b':show x|x<-[0..n-1]] ['z':show x|x<-[0..n-1]]
        orOutOfPlace  n = mkBitwiseOR  ['a':show x|x<-[0..n-1]] ['b':show x|x<-[0..n-1]] ['z':show x|x<-[0..n-1]]
        xorOutOfPlace n = mkBitwiseXOR ['a':show x|x<-[0..n-1]] ['b':show x|x<-[0..n-1]] ['z':show x|x<-[0..n-1]]


        

writeCircuit :: String-> Circuit -> IO()
writeCircuit fname circ = do 
  writeFile (fname++".qc") (show circ)
  _ <- runX ( circToXML fname circ )
  return() 
