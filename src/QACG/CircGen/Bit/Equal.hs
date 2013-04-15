module QACG.CircGen.Bit.Equal
( equal
  ,mkEqualOutOfPlace
  ,mkNotEqualOutOfPlace
) where

import QACG.CircUtils.Circuit
import QACG.CircUtils.CircuitState
import Control.Monad.State
import Control.Exception

import QACG.CircGen.Bit.MCToff
import QACG.CircGen.Bit.Toffoli

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

notEqual x y targ = do equal x y targ 
                       notgate targ

mkEq :: ([String] -> [String] -> String -> CircuitState ()) -> [String]-> [String] -> String -> Circuit
mkEq f a b targ = circ
  where (_,(_,_,circ)) = runState go ([], map (\x->'c':show x) [0..(3 * length a)] , Circuit (LineInfo [] [] [] []) [] [])
        go             = do as <- initLines a
                            bs <- initLines b
                            t <- initLines [targ]
                            f a b (head t)
                            setOutputs $ as ++ bs ++ t

mkEqualOutOfPlace, mkNotEqualOutOfPlace
    :: [String]-> [String] -> String -> Circuit

mkEqualOutOfPlace = mkEq equal
mkNotEqualOutOfPlace = mkEq notEqual
