-- | A reversible cicuit for integer division
module QACG.CircGen.Div.SimpleInt
( genSimpleInt
) where

import Data.List
import QACG.CircUtils.Circuit

import QACG.CircGen.Add.SimpleRipple
import QACG.CircGen.Comp.Ripple

genSimpleInt :: Int -> Circuit 
genSimpleInt n = Circuit { lineInfo = cLines n, gates = cGates n 0, subcircuits= cSubs n}


var vName n = [ vName:show x | x<-[0..n-1] ] 

cSubs :: Int -> [(Circuit,String)]
cSubs n = [subtract,shift,comp]
  where subtract = (mkSimpleSubtract (var 'a' n) (var 'b' n), "SUB")
        shift = (genLeftShift (n-1), "SHIFT")
        comp = (mkGreaterOutOfPlace  (var 'a' n) (var 'b' n) "z", "COMPARE")

cLines :: Int -> LineInfo 
cLines n = 
  LineInfo { vars = cVars
  , inputs = cInputs
  , outputs = cOutputs 
  , outputLabels = cOutLab }
  where cInputs = number n ++ divisor n
        cOutputs = cInputs ++ ["c"] ++ quotient n ++ remainder n ++ ["cComp"]
        cOutLab = cInputs ++ ["0"] ++quotient n ++ remainder n  ++ ["0"]
        cVars = cOutputs

cGates :: Int -> Int -> [Gate]
cGates n x | x == n  = []
           | otherwise = Gate "SHIFT" (remainder n) 
               : cnot ('N':show x) ('R':show x) 
               : Gate "COMPARE" (["c"] ++ remainder n ++ divisor n ++ ["cComp"])
               : Gate "SUB" ( ["c"] ++ remainder n ++ divisor n )
               : cnot "cComp" ('Q':show x) 
               : cnot ('Q':show x) "cComp" 
               : cGates n (x+1)

number n = [ x++y | x <- ["N"] , y <- map show [0..(n-1)]]
divisor n = [ x++y | x <- ["D"] , y <- map show [0..(n-1)]]
quotient n = [ x++y | x <- ["Q"] , y <- map show [0..(n-1)]]
remainder n = [ x++y | x <- ["R"] , y <- map show [0..(n-1)]]


cnot :: String -> String -> Gate
cnot a b = Gate "TOF" [a,b]

genLeftShift :: Int -> Circuit 
genLeftShift n = (Circuit { lineInfo = shiftCLines n, gates = shiftCGates n, subcircuits= []})

shiftCLines :: Int -> LineInfo 
shiftCLines n = 
  LineInfo { vars = cVars
  , inputs = cInputs
  , outputs = cOutputs 
  , outputLabels = cOutLab }
  where cInputs = zipWith (++) (repeat "b") (map show [0..(n-1)])
        cOutputs = cInputs ++ ['b' : show n]
        cOutLab = "0" : cInputs
        cVars = cOutputs

shiftCGates :: Int -> [Gate]
shiftCGates 1 = [ cnotS 0 1 , cnotS 1 0 ]
  where cnotS a b = Gate "tof" ["b" ++ show a , "b" ++ show b]
shiftCGates n = [ cnotS (n-1) n , cnotS n (n-1)] ++ shiftCGates (n-1)
  where cnotS a b = Gate "tof" ["b" ++ show a , "b" ++ show b]
