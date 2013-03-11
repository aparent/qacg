module CircGen.Bit.Shift
( equal
  ,mkEqual
) where

import CircUtils.Circuit
import CircUtils.CircuitState
import Control.Monad.State
import CircGen.Bit.MCToff
import Control.Exception

equal :: [String] -> [String] -> String -> CircuitState ()
equal x y targ = assert (length x == length y) $ go x y
  where go (a:as) (b:bs) =  do cnot a b 
                               go as bs
                               cnot a b
        go [] [] = do applyNots y 
                      mcToff y targ
                      applyNots y
        applyNots (l:ls) = do notgate l
                              applyNots ls
        applyNots [] = return ()

mkEqual :: [String]-> [String] -> String -> Circuit
mkEqual a b targ = circ
  where (_,(_,_,circ)) = runState go ([], map (\x->'c':show x) [0..(3 * length a)] , Circuit (LineInfo [] [] [] []) [] [])
        go             = do as <- initLines a
                            bs <- initLines b
                            t <- initLines [targ]
                            equal a b (head t)
                            setOutputs $ as ++ bs ++ t

