-- Used muliplier from here converted into a reversible circuit http://www.fpga-guru.com/multipli.htm

import System.Environment
import Data.List
import System.IO
import CircUtils.Circuit

main = do  
   args <- getArgs
   if (read $ head args) < 3
	then putStrLn "ERROR: must be at least size 3"
   else do
   	putStrLn $ write_qc $ gen_ripple_multiplier $ read $ head args

--This section generates the partial product circuit for use in the muliplier.  Note this may be useful in other muliplier circuits
gen_partial_product :: Int -> Circuit
gen_partial_product a = (Circuit { line_info = partial_lines a, gates = partial_gates a, subcircuits= []})

partial_lines :: Int -> Line_Info 
partial_lines a = 
	Line_Info { vars = partial_inputs a ++ partial_ancilla a
                  , inputs = partial_inputs a  
                  , outputs = partial_inputs a ++ partial_ancilla a
                  , output_labels = partial_inputs a ++ partial_ancilla a  }

partial_inputs ::  Int -> [String]
partial_inputs a = zipWith (++) (take (a*2) (cycle ["a","b"])) (concatMap (replicate 2) (map show [0..a])) 

partial_ancilla :: Int -> [String]
partial_ancilla a =  let blist = take a (zipWith (++) (repeat "b") $ map show [0..])
                         alist = take a (zipWith (++) (repeat "a") $ map show [0..])
		in  map (\(x,y) -> x++y) $ sortBy partial_line_sort $ concatMap (\x -> map (\y -> (x,y)) blist) alist --sort so they are grouped better for adders 

partial_gates :: Int -> [Gate]
partial_gates a = let blist = take a (zipWith (++) (repeat "b") $ map show [0..])
                      alist = take a (zipWith (++) (repeat "a") $ map show [0..])
		in map (\x-> Gate "TOF" x) (concatMap (\x-> map (\y-> [x,y,x++y]) blist) alist)

partial_line_sort :: (String,String) -> (String,String) -> Ordering
partial_line_sort  (_:x1,_:y1) (_:x2, _:y2)  = compare ((read x1)+(read y1)) ((read x2)+(read y2))

--------------------------------------------------------------------------------------------------------
fullAdder = Circuit (Line_Info ["x","y","0","c"] [] [] [])  ([Gate "tof" ["x","c","0"], Gate "tof" ["x","c"],Gate "tof" ["y","c","0"], Gate "tof" ["y","c"]]) []
halfAdder = Circuit (Line_Info ["x","y","c"] [] [] [])  ([Gate "tof" ["x","y"], Gate "tof" ["x","y","c"]]) []


gen_ripple_multiplier :: Int -> Circuit
gen_ripple_multiplier size = (Circuit { line_info = mult_lines size, gates = mult_gates size, subcircuits= (gen_partial_product size,"PP"):(fullAdder,"FA"):(halfAdder,"HA"):[]})

mult_lines :: Int -> Line_Info
mult_lines size = partial_lines size

mult_gates :: Int -> [Gate]
mult_gates size =  mult_adder_lines size 0 (sortBy (\(x1,y1) (x2,y2) -> compare (x1+y1) (x2+y2)) $ concatMap (\x -> map (\y -> (x,y)) [0..size]) [0..size])

mult_adder_lines ::Int -> Int-> [(Int,Int)] -> [Gate]
mult_adder_lines size n [] = mult_adder_lines size (n+1) pairs
mult_adder_lines size n pairs | n < size*2 - 1 = mult_adders(filter (\(x,y) -> x+y == n) pairs)  ++ mult_adder_lines size (n+1) pairs
			      | otherwise = []

mult_adders :: [(Int,Int)] -> [Gate]
mult_adders pairs | length pairs < 2 = []
	          | mod (length pairs) 3 == 1 = (Gate "HA" [g1,g2])    : mult_adders (tail$tail pairs)
	          | otherwise                 = (Gate "FA" [g1,g2,g3]) : mult_adders (tail$tail$tail pairs)
		  where l1 = head pairs
			l2 = head $ tail pairs
			l3 = head $ tail $ tail pairs
			g1 = "a"++show(fst l1) ++ "b" ++ show (snd l1)	 
			g2 = "a"++show(fst l2) ++ "b" ++ show (snd l2)	 
			g3 = "a"++show(fst l3) ++ "b" ++ show (snd l3)	 
