module CircGen.Add.Ripple
( genRipple
) where

import Data.List
import CircUtils.Circuit


-- |Generates the addition circuit in <http://arxiv.org/abs/quant-ph/0410184> and returns it as a circuit
genRipple :: Int -> Circuit 
genRipple a = (Circuit { line_info = adder_lines a, gates = adder_gates a, subcircuits= []})

adder_lines :: Int -> Line_Info 
adder_lines a = 
	Line_Info { vars = adder_inputs a 0  ++ ["z"]
                  , inputs = adder_inputs a 0  ++ ["z"] 
                  , outputs = adder_inputs a 0 ++ ["z"] 
                  , output_labels = adder_outputs a 0 ++ ["z"]}

adder_inputs :: Int -> Int -> [String]
adder_inputs n x | x == 0 = "b0": "a0": "c" : adder_inputs n (x+1)
	         |  x < n    = ("b" ++ show x):("a" ++ show x):adder_inputs n (x+1) 
                 | otherwise = []

adder_outputs :: Int -> Int -> [String]
adder_outputs n x | x == 0 = "s0": "a0": "c" : adder_outputs n (x+1)
	          |  x < n    = ("s" ++ show x):("a" ++ show x):adder_outputs n (x+1) 
                  | otherwise = []

start_left = [Gate "tof" ["a1","c"],Gate "tof" ["a0","b0","c"],Gate "tof" ["a2","a1"],Gate "tof" ["c", "b1", "a1"]]

adder_gates :: Int -> [Gate]
adder_gates a =  startCNots ++ start_left ++ left a 3 ++ middle (a-1) ++ right (a-1) ++ endCNots
  where startCNots = map (\x -> cnots x) [1..(a-1)] 
        endCNots = map (\x -> cnots x) [0..(a-1)] 
        cnots x = Gate "tof" ["a" ++ show x, "b" ++ show x]

left :: Int -> Int -> [Gate]
left n x | x < n = (Gate "tof" ["a" ++ show x , "a" ++ show (x-1)])
                   :(Gate "tof" ["a" ++ show (x-2) , "b" ++ show (x-1),  "a" ++ show (x-1) ])
		   :left n (x+1)
          | otherwise = []

middle :: Int -> [Gate]
middle n = (Gate "tof" ["a" ++ show (n) , "z"]):(Gate "tof" ["a" ++ show (n-1), "b" ++ show (n), "z"]) : middleNots n ++ middleCNots n 
  where middleNots n = map (\x -> nots x) [1..(n-1)] 
        middleCNots n = Gate "tof" ["c","b1"] : map (\x -> cnots x) [1..(n-1)]
        cnots x = Gate "tof" ["a" ++ show x, "b" ++ show (x+1)]
        nots x = Gate "tof" ["b" ++ show x]

right :: Int -> [Gate]
right 2 = [Gate "tof" ["c", "b1", "a1"], Gate "tof" ["a2","a1"], Gate "tof" ["b1"],Gate "tof" ["b0", "a0", "c"],Gate "tof" ["a1","c"]]
right x =  Gate "tof" ["a" ++ show (x-2), "b" ++ show (x-1), "a" ++ show (x-1)]: 
           Gate "tof" ["b" ++ show (x-1)]:
           Gate "tof" ["a" ++ show x,"a" ++ show (x-1)]:
           right (x-1)
