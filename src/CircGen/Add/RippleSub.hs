-- | Subtractor circuit from <http://arxiv.org/abs/quant-ph/0410184>
module CircGen.Add.RippleSub
( genRippleSub
) where

import Data.List
import CircUtils.Circuit

import CircGen.Add.Ripple


genRippleSub :: Int -> Circuit 
genRippleSub n = (Circuit { line_info = cLines n, gates = cGates n, subcircuits= []})

cLines :: Int -> Line_Info 
cLines n = 
  Line_Info { vars = cVars
  , inputs = cInputs
  , outputs = cOutputs 
  , output_labels = cOutLab }
  where cInputs = [ x++y | x <- ["a","b"] , y <- map show [0..(n-1)]]
        cOutputs = "c" : cInputs ++ ["z"]
        cOutLab = "0" : cInputs ++ ["z"]
        cVars = cOutputs

cGates :: Int -> [Gate]
cGates n = beginNots ++  (gates $ genRipple n) ++ endNots
  where beginNots = map (\x-> Gate "tof" ["a"++show x]) [0..(n-1)]
        endNots = concatMap (\x-> [Gate "tof" ["a"++show x],Gate "tof"["b"++show x]]) [0..(n-1)]
