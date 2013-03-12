-- | Contains out of place bitwise operations 
module CircGen.Bit.BitwiseOP
( bitwiseAND
  ,bitwiseOR
  ,bitwiseXOR
  ,mkBitwiseAND
  ,mkBitwiseOR
  ,mkBitwiseXOR

) where

import CircUtils.Circuit
import CircUtils.CircuitState
import Control.Monad.State
import Control.Exception

import CircGen.Bit.Toffoli

bitwise :: (String -> String -> String -> CircuitState ()) -> [String] -> [String] -> [String] ->  CircuitState ()
bitwise f a b c = assert (length a == length b && length a == length c) $ go a b c 
  where go (x:xs) (y:ys) (z:zs)  = do f x y z
                                      go xs ys zs
        go [] [] [] = return ()

bitAND :: String -> String -> String -> CircuitState ()  
bitAND = tof 

bitOR :: String -> String -> String -> CircuitState ()  
bitOR a b c = do tof a b c
                 cnot a c
                 cnot b c

bitXOR :: String -> String -> String -> CircuitState ()  
bitXOR a b c = do cnot a c
                  cnot b c 

bitwiseAND = bitwise bitAND
bitwiseOR = bitwise bitOR
bitwiseXOR = bitwise bitXOR

mkBitwise:: ([String] -> [String] -> [String] ->  CircuitState ()) ->  [String] -> [String] -> [String] -> Circuit
mkBitwise f a b c = circ
  where (_,(_,_,circ)) = runState go ([],  ['c':show x | x <- [0 .. 4]], Circuit (LineInfo [] [] [] []) [] [])
        go             = do initLines a
                            initLines b 
                            initLines c
                            f a b c
                            setOutputs $ a ++ b ++ c


mkBitwiseAND = mkBitwise bitwiseAND
mkBitwiseOR = mkBitwise bitwiseOR
mkBitwiseXOR = mkBitwise bitwiseXOR
