module CircGen.Bit.Shift
( and
  ,mkBitwiseAnd
) where

import CircUtils.Circuit
import CircUtils.CircuitState
import Control.Monad.State
import CircGen.Bit.MCToff
import Control.Exception

bitwiseAnd :: [String] -> [String] -> String -> CircuitState ()
bitwiseAnd a b targ = assert (length a == length b) $ go a b []
  where go (a:as) (b:bs) cs =  do c <- getConst 1 
                                  tof [a,b,head c]
                                  go as bs (head c : cs)
                                  tof [a,b,head c]
                                  freeConst c
        go [] [] cs = mcToff cs targ

mkBitwiseAnd:: [String]-> [String] -> String -> Circuit
mkBitwiseAnd a b targ = circ
  where (_,(_,_,circ)) = runState go ([], map (\x->'c':show x) [0..(2 * length a)] , Circuit (LineInfo [] [] [] []) [] [])
        go             = do as <- initLines a
                            bs <- initLines b
                            t <- initLines [targ]
                            bitwiseAnd a b (head t)
                            setOutputs $ as ++ bs ++ t

