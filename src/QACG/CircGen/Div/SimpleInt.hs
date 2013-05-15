-- | A reversible cicuit for integer division
module QACG.CircGen.Div.SimpleInt
( genSimpleInt
) where

import Data.List
import CircUtils.Circuit

import QACG.CircGen.Add.RippleSub
import QACG.CircGen.Comp.Ripple
import QACG.CircGen.Bit.Shift

genSimpleInt :: Int -> Circuit 
genSimpleInt n = Circuit { line_info = cLines n, gates = cGates n 0, subcircuits= cSubs n}


cSubs :: Int -> [(Circuit,String)]
cSubs n = [subtract,shift,comp]
  where subtract = (genRippleSub n, "SUB")
        shift = (genLeftShift (n-1), "SHIFT")
        comp = ( genRipple n, "COMPARE")

cLines :: Int -> Line_Info 
cLines n = 
  Line_Info { vars = cVars
  , inputs = cInputs
  , outputs = cOutputs 
  , output_labels = cOutLab }
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
