module CircGen.Bit.LeftShift
( genLeftShift
) where

import CircUtils.Circuit
import Data.List (nub, zipWith)

genLeftShift :: Int -> Circuit 
genLeftShift n = (Circuit { line_info = cLines n, gates = cGates n, subcircuits= []})

cLines :: Int -> Line_Info 
cLines n = 
  Line_Info { vars = cVars
  , inputs = cInputs
  , outputs = cOutputs 
  , output_labels = cOutLab }
  where cInputs = zipWith (++) (repeat "b") (map show [0..(n-1)])
        cOutputs = cInputs ++ ['b' : show n]
        cOutLab = "0" : cInputs
        cVars = cOutputs

cGates :: Int -> [Gate]
cGates 1 = [ cnot 0 1 , cnot 1 0 ]
cGates n = [ cnot (n-1) n , cnot n (n-1)] ++ cGates (n-1)

cnot :: Int -> Int -> Gate
cnot a b = Gate "tof" ["b" ++ show a , "b" ++ show b]
