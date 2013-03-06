module CircGen.Bit.MCToff
( mcToff
  ,mkMcToff
) where


import CircUtils.Circuit
import CircUtils.CircuitState
import Control.Monad.State

mcToff :: [String] -> String -> CircuitState ()
mcToff conts targ = do reduceCont conts []
  where reduceCont (x:y:xs) red = do c <- getConst 1
                                     tof [x,y,head c]    
                                     reduceCont xs $ head c : red
                                     tof [x,y,head c]    
                                     freeConst c
        reduceCont (x:[]) red = reduceCont [] (x:red)
        reduceCont []     red | length red > 1 = reduceCont red []
                              | otherwise = tof [head red,targ]


mkMcToff:: [String] -> String -> Circuit
mkMcToff controls target = circ
  where (_,(_,_,circ)) = runState go ([], map (\x->'c':show x) [0..length controls] , Circuit (LineInfo [] [] [] []) [] [])
        go             = do initLines controls
                            mcToff controls target
                            initLines [target]
                            setOutputs $ controls ++ [target]

