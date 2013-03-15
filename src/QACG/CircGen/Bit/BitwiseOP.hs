-- | Contains out of place bitwise operations 
module QACG.CircGen.Bit.BitwiseOP
( bitwiseAND
  ,bitwiseOR
  ,bitwiseXOR
  ,mkBitwiseAND
  ,mkBitwiseOR
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

bitAND :: String -> String -> String -> CircuitState ()  
bitAND = tof 

bitOR :: String -> String -> String -> CircuitState ()  
bitOR a b c = do tof a b c
                 cnot a c
                 cnot b c

bitXOR :: String -> String -> String -> CircuitState ()  
bitXOR a b c = do cnot a c
                  cnot b c 
bitwiseAND :: [String] -> [String] -> [String] ->  CircuitState ()
bitwiseAND = bitwise bitAND

bitwiseOR :: [String] -> [String] -> [String] ->  CircuitState ()
bitwiseOR = bitwise bitOR

bitwiseXOR :: [String] -> [String] -> [String] ->  CircuitState ()
bitwiseXOR = bitwise bitXOR

mkBitwise:: ([String] -> [String] -> [String] ->  CircuitState ()) ->  [String] -> [String] -> [String] -> Circuit
mkBitwise f a b c = circ
  where (_,(_,_,circ)) = runState go ([],  ['c':show x | x <- [0 .. 4]], Circuit (LineInfo [] [] [] []) [] [])
        go             = do _ <- initLines a
                            _ <- initLines b 
                            _ <- initLines c
                            f a b c
                            setOutputs $ a ++ b ++ c


mkBitwiseAND :: [String] -> [String] -> [String] -> Circuit
mkBitwiseAND = mkBitwise bitwiseAND

mkBitwiseOR :: [String] -> [String] -> [String] -> Circuit
mkBitwiseOR = mkBitwise bitwiseOR

mkBitwiseXOR :: [String] -> [String] -> [String] -> Circuit
mkBitwiseXOR = mkBitwise bitwiseXOR
