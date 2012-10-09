module CircGen.Add.CarrySelect
( genCarrySelect
) where

import Data.List
import CircUtils.Circuit
import CircGen.Add.SimpleRipple

genCarrySelect :: Int -> Int -> Circuit 
genCarrySelect a b = Circuit { line_info = adder_lines a (b-1), gates = adder_gates a (b-1), subcircuits= gen_adders a ++ gen_mult ((a)*2+1) }

-- | n: number of gates per adder
-- | b: number of adders
gen_adders :: Int -> [(Circuit,String)]
gen_adders a = (genSimpleRipple a , "ADD" ++ show a )
              :(Circuit (line_info $ genSimpleRipple a) (reverse $ gates $ genSimpleRipple a) [] , "IADD" ++ show a ):[]


gen_mult :: Int -> [(Circuit,String)]
gen_mult x = [(Circuit (mult_lines (x)) (mult_gates (x-1)) [] ,"MUX" ++ show(x))]

mult_lines :: Int -> Line_Info 
mult_lines x = Line_Info (mult_inputs x 0) [] [] []

mult_inputs :: Int -> Int -> [String]
mult_inputs n x | x == 0 = "c" : "b0": "a0": mult_inputs n (x+1)
		|  x < n    = ("b" ++ l):("a" ++ l) : mult_inputs n (x+1) 
                | otherwise = []
		where l = show x

mult_gates :: Int-> [Gate]
mult_gates x | x >=  0    = Gate "swap" ["c","a"++l,"b"++l] : mult_gates (x-1)
	    | otherwise = []
            where l = show x

adder_lines :: Int -> Int-> Line_Info 
adder_lines a b=  
	Line_Info { vars = adder_inputs a b 
                  , inputs = adder_inputs a b   
                  , outputs = adder_inputs a b  
                  , output_labels = adder_inputs a b }

adder_inputs :: Int -> Int -> [String] 
adder_inputs	_ (-1)  =  []
adder_inputs	n b  = adder_inputs n (b-1) ++  (map (\a -> a ++ ("a"++show b) ) (ripple_inputs n 0))
		        ++ (map (\a -> a ++ ("a"++show b) ) ( map (\a -> 'c':a) (ripple_inputs n 0) )) 


ripple_inputs :: Int -> Int -> [String] 
ripple_inputs n x | x == 0 = "c0": "b0": "a0" : ripple_inputs n (x+1)
	          |  x < n    = ("b" ++ show x):("a" ++ show x):ripple_inputs n (x+1) 
                  | otherwise = ["z"]

adder_gates :: Int -> Int -> [Gate]
adder_gates a b =  apply_copies a b "a" ++ apply_copies a b "b"++ apply_adders a b ++ apply_mux a b ++ apply_iadders a b 

apply_copies :: Int -> Int -> String -> [Gate]
apply_copies 0 b l = []
apply_copies a b l = (apply_gate_layer b "tof" [l++ (show $ a-1)++"a","c"++l++(show $ a-1) ++"a"]) ++ (apply_copies (a-1) b l)

apply_adders :: Int -> Int -> [Gate]
apply_adders a b = apply_gate_layer b ("ADD"++ show a) (map (\y-> y ++ "a") (ripple_inputs a 0)) 
 		   ++ apply_gate_layer b ("ADD"++ show a) (map (\y-> "c" ++ y ++ "a") (ripple_inputs a 0)) 

apply_iadders :: Int -> Int -> [Gate]
apply_iadders a b = apply_gate_layer b ("IADD"++ show a) (map (\y-> y ++ "a") (ripple_inputs a 0)) 



apply_mux :: Int -> Int -> [Gate]
apply_mux a b = (\x -> (head x): insert_cnots 0 (tail x)) $ reverse $ apply_gate_layer b ("MUX"++ show ((a)*2+1))  $ (\(x:y:xs) -> (x:xs)) $
                             reverse $ foldl (\y (c,d) -> c:d:y) [] $ zip (map (\y-> "c" ++ y ++ "a") (ripple_inputs a 0)) (map (\y-> y ++ "a") (ripple_inputs a 0))


insert_cnots :: Int -> [Gate] -> [Gate]
insert_cnots _ [] = []
insert_cnots n (x:xs) = (Gate "tof" ["cza"++(show n),"c0a"++(show $ n+1)]):x:(Gate "tof" ["cza"++(show n),"c0a"++(show $ n+1)]):(insert_cnots (n+1) xs)

apply_gate_layer :: Int -> String -> [String]-> [Gate]
apply_gate_layer (-1) _ _ = []
apply_gate_layer x n g = (Gate n $map (\a -> a ++ show x) g): apply_gate_layer (x-1) n g
