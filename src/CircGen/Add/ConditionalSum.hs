
import System.Environment
import Data.List
import System.IO
import Circuit

main = do  
   args <- getArgs
   if (read $ head args) < 3
	then putStrLn "ERROR: must be at least size 3"
   else do
   	putStrLn $ write_qc $ gen_ripple_adder $ read $ head args


gen_ripple_adder :: Int -> Circuit 
gen_ripple_adder a = Circuit { line_info = adder_lines a, gates = adder_gates a, subcircuits= (fullAdder,"ADD"):(gen_mult $ (1 +  (floor $ logBase 2 (fromIntegral a:: Double))))}

gen_mult :: Int -> [(Circuit,String)]
gen_mult 0 = []
gen_mult x = (Circuit (mult_lines $ floor 2^x) (mult_gates $ (floor 2^x)-1 ) [] ,"MUX" ++ show(2^x)):gen_mult(x-1)

mult_lines :: Int -> Line_Info 
mult_lines x = Line_Info (mult_inputs x 0) [] [] []

mult_inputs :: Int -> Int -> [String]
mult_inputs n x | x == 0 = "c" : "b0": "a0": mult_inputs n (x+1)
		|  x < n    = ("b" ++ l):("a" ++ l) : mult_inputs n (x+1) 
                | otherwise = []
		where l = show x

mult_gates :: Int-> [Gate]
mult_gates x | x >= 0    = Gate "swap" ["c","a"++l,"b"++l] : mult_gates (x-1)
	    | otherwise = []
            where l = show x

adder_lines :: Int -> Line_Info 
adder_lines a =  
	Line_Info { vars = adder_inputs a 0  ++ ["z"]
                  , inputs = adder_inputs a 0  ++ ["z"] 
                  , outputs = adder_inputs a 0 ++ ["z"] 
                  , output_labels = adder_outputs a 0 ++ ["z"]}

adder_inputs :: Int -> Int -> [String]
adder_inputs n x | x == 0 = "c0": "s0":"b0": "a0": adder_inputs n (x+1)
	         |  x < n    = ("b" ++ l):("a" ++ l):("s"++l):("c"++l):("cb"++l):("ca"++l):("cs"++l):("cc"++l):(adder_inputs n (x+1))
                 | otherwise = []
		 where l = show x

adder_outputs :: Int -> Int -> [String]
adder_outputs n x | x == 0 = "c0": "s0": "b0":"a0" : adder_outputs n (x+1)
	          |  x < n    = ("b" ++ l):("a" ++ l):("s"++l):("c"++l):("cb"++l):("ca"++l):("cs"++l):("cc"++l):(adder_outputs n (x+1))
                  | otherwise = []
		 where l = show x

adder_gates :: Int -> [Gate]
adder_gates a =  (Gate "ADD" ["a0","b0","s0","c0"]):(adder_copy a ++ adder_add a ++ adder_mult a 2)

adder_copy  a = ((apply_gate_layer (a-1) "tof" ["a","ca"]) ++ (apply_gate_layer (a-1) "tof" ["b","cb"]))
adder_add  a = ((apply_gate_layer (a-1) "ADD" ["a","b","s","c"]) ++ (apply_gate_layer (a-1) "ADD" ["cb","ca","cs","cc"]))

adder_mult:: Int -> Int -> [Gate]
adder_mult x s| s < div x 2 = apply_mult_layer x s ++ adder_mult x (s*2)
	      | otherwise = []

apply_mult_layer:: Int -> Int -> [Gate]
apply_mult_layer  x s | x - s > 0  = 
	(Gate ("MUX"++(show s)) ( ("c"++(show (x-s*2))) : (zipWith (++) (take ( s*2 ) (cycle ["a","ca","b","cb"])) (map show [div (x+1) 4 | x <- [4*(x-s)..4*x ]]))))
	:apply_mult_layer (x-s) s
	              | otherwise = []


adder_mult_cleanup:: Int -> [Gate]
adder_mult_cleanup a = []


apply_gate_layer :: Int -> String -> [String]-> [Gate]
apply_gate_layer 0 _ _ = []
apply_gate_layer x n g = (Gate n $map (++ show x) g): apply_gate_layer (x-1) n g

fullAdder = Circuit (Line_Info ["x","y","0","c"] [] [] [])  ([Gate "tof" ["x","c","0"], Gate "tof" ["x","c"],Gate "tof" ["y","c","0"], Gate "tof" ["y","c"]]) []
