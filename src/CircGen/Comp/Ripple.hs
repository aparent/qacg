-- | Comparison circuit from <http://arxiv.org/abs/quant-ph/0410184>
module CircGen.Comp.Ripple
( genRipple
) where

import Data.List
import CircUtils.Circuit

genRipple :: Int -> Circuit 
genRipple n = (Circuit { line_info = cLines n, gates = cGates n, subcircuits= []})

cLines :: Int -> Line_Info 
cLines n = 
  Line_Info { vars = cVars
  , inputs = cInputs
  , outputs = cOutputs 
  , output_labels = cOutLab }
  where cInputs = [ x++y | x <- ["a","b"] , y <- map show [0..(n-1)]]
        cOutputs = "c" : cInputs ++ ["res"]
        cOutLab = "0" : cInputs ++ ["res"]
        cVars = cOutputs

cGates :: Int -> [Gate]
cGates n = start ++ middle ++ reverse start
  where middle = [cnot ("a"++show (n-1)) "res", tof ("a"++show (n-2)) ("b"++show (n-1)) "res"]
        start = nots ++ cnots ++ begin ++ ripple
          where nots = map (\x-> Gate "tof" ["a"++show x]) [0..(n-1)]
                cnots = map (\x-> cnot ("a"++show x) ("b"++show x)) [1..(n-1)]
                begin = [cnot "a1" "c"
                         ,tof "b0" "a0" "c"
                         ,cnot "a2" "a1"
                         ,tof "c" "b1" "a1"]
                ripple = concatMap cnotLevel [1..(n-3)] 
                  where cnotLevel x = [ cnot ("a"++show(x+2)) ("a"++show (x+1)),
                                        tof ("a"++show x) ("b"++show (x+1))("a"++show (x+1))]

cnot :: String -> String -> Gate
cnot a b = Gate "tof" [a,b]

tof :: String -> String -> String -> Gate
tof a b c = Gate "tof" [ a , b, c]
