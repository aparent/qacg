module CircGen.Add.SimpleRipple
( simpleRipple
 ,simpleRippleCtrl
 ,applySimpleRipple
 ,applySimpleRippleCtrl
) where

import CircUtils.Circuit

-- |Generates the addition circuit in <http://arxiv.org/abs/quant-ph/0410184> and returns it as a circuit
simpleRipple :: Int -> Circuit 
simpleRipple a = Circuit (adderLines a) (adderGates a) []

simpleRippleCtrl :: Int -> Circuit
simpleRippleCtrl a = Circuit (adderLinesCtrl a) (adderGatesCtrl a) []

applySimpleRipple :: [String] -> [String] -> String -> String -> [Gate]
applySimpleRipple a b z c = init $ go (c:a) b --Last gate is not needed since c = 0
  where go (a:[]) [] = [Gate "tof" [a,z]]
        go (a0:a1:as) (b0:bs) = maj a0 b0 a1 ++ go (a1:as) bs ++ uma a0 b0 a1  
        maj x y z  = [Gate "tof" [z,y], Gate "tof" [z,x],Gate "tof" [x,y,z]]
        uma x y z  = [Gate "tof" [x,y,z], Gate "tof" [z,x],Gate "tof" [x,y]]

applySimpleRippleCtrl :: String -> [String] -> [String] -> String -> String -> [Gate]
applySimpleRippleCtrl ctrl a b z c = init $ go (c:a) b  --Last gate is not needed since c = 0
  where go (a:[]) [] = [Gate "tof" [ctrl,a,z]]
        go (a0:a1:as) (b0:bs) = maj a0 b0 a1 ++ go (a1:as) bs ++ uma a0 b0 a1  
        maj x y z  = [Gate "tof" [ctrl,z,y], Gate "tof" [z,x],Gate "tof" [x,y,z]]
        uma x y z  = [Gate "tof" [x,y,z], Gate "tof" [z,x],Gate "tof" [ctrl,x,y]]


adderLines :: Int -> LineInfo 
adderLines a = LineInfo vars inputs outputs outputLab
  where adderInputs  n = "c" : concatMap (\x->['b':show x,'a':show x]) [0..(n-1)]
        adderOutputs n = "c" : concatMap (\x->['s':show x,'a':show x]) [0..(n-1)]
        vars = adderInputs a ++ ["z"]
        inputs = vars 
        outputs = inputs
        outputLab = adderOutputs a ++ ["z"]

adderLinesCtrl :: Int -> LineInfo
adderLinesCtrl n = LineInfo ("ctrl":v) ("ctrl":i) ("ctrl":o) ("ctrl":ol)
  where (LineInfo v i o ol) = adderLines n

adderGatesCtrl :: Int -> [Gate]
adderGatesCtrl n = applySimpleRippleCtrl "ctrl" a b "z" "c"
	where a = map (\x -> 'a':show x) [0..(n-1)]
	      b = map (\x -> 'b':show x) [0..(n-1)]

adderGates :: Int -> [Gate]
adderGates n = applySimpleRipple a b "z" "c"
	where a = map (\x -> 'a':show x) [0..(n-1)]
	      b = map (\x -> 'b':show x) [0..(n-1)]
