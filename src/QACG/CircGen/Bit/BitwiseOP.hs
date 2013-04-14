-- | Contains out of place bitwise operations 
module QACG.CircGen.Bit.BitwiseOP
( bitwiseAND
  ,bitwiseNAND
  ,bitwiseOR
  ,bitwiseNOR
  ,bitwiseXOR
  ,mkBitwiseAND
  ,mkBitwiseNAND
  ,mkBitwiseOR
  ,mkBitwiseNOR
  ,mkBitwiseXOR

) where

import QACG.CircUtils.Circuit
import QACG.CircUtils.CircuitState
import Control.Monad.State
import Control.Exception

import QACG.CircGen.Bit.Toffoli

bitwise :: (String -> String -> String -> CircuitState ()) -> [String] -> [String] -> [String] ->  CircuitState ()
bitwise f a b c = assert (length a == length b && length a == length c) $ go a b c 
  where go (x:xs) (y:ys) (z:zs)  = do f x y z
                                      go xs ys zs
        go [] [] [] = return ()
        go _ _ _ = assert False $ return () --Should never happen!

bitNAND,bitAND,bitOR,bitNOR,bitXOR 
  :: String -> String -> String -> CircuitState ()  
bitAND = tof 
bitNAND a b c = do bitAND a b c 
                   notgate c
bitOR a b c = do tof a b c
                 cnot a c
                 cnot b c
bitNOR a b c = do bitOR a b c
                  notgate c
bitXOR a b c = do cnot a c
                  cnot b c 

bitwiseAND,bitwiseNAND,bitwiseOR,bitwiseNOR,bitwiseXOR 
    :: [String] -> [String] -> [String] ->  CircuitState ()
bitwiseAND = bitwise bitAND
bitwiseNAND = bitwise bitNAND
bitwiseOR = bitwise bitOR
bitwiseNOR = bitwise bitNOR
bitwiseXOR = bitwise bitXOR

mkBitwise:: ([String] -> [String] -> [String] ->  CircuitState ()) ->  [String] -> [String] -> [String] -> Circuit
mkBitwise f a b c = circ
  where (_,(_,_,circ)) = runState go ([],  ['c':show x | x <- [0 .. 4]], Circuit (LineInfo [] [] [] []) [] [])
        go             = do _ <- initLines a
                            _ <- initLines b 
                            _ <- initLines c
                            f a b c
                            setOutputs $ a ++ b ++ c


mkBitwiseAND,mkBitwiseNAND,mkBitwiseOR,mkBitwiseNOR,mkBitwiseXOR
    :: [String] -> [String] -> [String] -> Circuit
mkBitwiseAND = mkBitwise bitwiseAND
mkBitwiseNAND = mkBitwise bitwiseNAND
mkBitwiseOR = mkBitwise bitwiseOR
mkBitwiseNOR = mkBitwise bitwiseNOR
mkBitwiseXOR = mkBitwise bitwiseXOR
