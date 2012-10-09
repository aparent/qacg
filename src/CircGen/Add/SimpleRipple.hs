module CircGen.Add.SimpleRipple
( simpleRipple
 ,simpleRippleCtrl
) where

import CircUtils.Circuit

-- |Generates the addition circuit in <http://arxiv.org/abs/quant-ph/0410184> and returns it as a circuit
simpleRipple :: Int -> Circuit 
simpleRipple a = Circuit (adderLines a) (adderGates a) []

simpleRippleCtrl :: Int -> Circuit
simpleRippleCtrl a = Circuit (adderLinesCtrl a) (adderGatesCtrl a) []

adderLines :: Int -> LineInfo 
adderLines a = LineInfo vars inputs outputs outputLab
  where adderInputs  n = "c": "b0": "a0" : concatMap (\x -> ['b':show x,'a':show x]) [1..(n-1)]
        adderOutputs n = "c": "s0": "a0" : concatMap (\x -> ['s':show x,'a':show x]) [1..(n-1)]
        vars = adderInputs a ++ ["z"]
        inputs = vars 
        outputs = inputs
        outputLab = adderOutputs a ++ ["z"]

adderLinesCtrl :: Int -> LineInfo
adderLinesCtrl n = LineInfo ("ctrl":v) ("ctrl":i) ("ctrl":o) ("ctrl":ol)
  where (LineInfo v i o ol) = adderLines n

adderGatesCtrl :: Int -> [Gate]
adderGatesCtrl a = maj "c" "b0" "a0" ++ left (a-2)++ [Gate "tof" ["ctrl",'a':show(a-1),"z"]] ++ right (a-2) ++ uma "c" "b0" "a0"
 where left n   = concatMap (\x -> maj ('a':show x) ('b':show (x+1)) ('a':show (x+1))) [0..n]
       right n  = concatMap (\x -> uma ('a':show x) ('b':show (x+1)) ('a':show (x+1))) $ reverse [0..n]
       maj a b c  = [Gate "tof" ["ctrl",c,b], Gate "tof" [c,a],Gate "tof" [a,b,c]]
       uma a b c  = [Gate "tof" [a,b,c], Gate "tof" [c,a],Gate "tof" ["ctrl",a,b]]

adderGates :: Int -> [Gate]
adderGates a = maj "c" "b0" "a0" ++ left (a-2)++ [Gate "tof" ['a':show(a-1),"z"]] ++ right (a-2) ++ uma "c" "b0" "a0"
 where left n   = concatMap (\x -> maj ('a':show x) ('b':show (x+1)) ('a':show (x+1))) [0..n]
       right n  = concatMap (\x -> uma ('a':show x) ('b':show (x+1)) ('a':show (x+1))) $ reverse [0..n]
       maj a b c  = [Gate "tof" [c,b], Gate "tof" [c,a],Gate "tof" [a,b,c]]
       uma a b c  = [Gate "tof" [a,b,c], Gate "tof" [c,a],Gate "tof" [a,b]]
