module CircGen.Bit.Toffoli
( tof
  ,tofMatchedD1
  ,leftTof
  ,rightTof
) where

import CircUtils.CircuitState
import Control.Exception(assert)

tof :: String -> String -> String -> CircuitState () 
tof a b c = do consts <- getConst 4
               assert (length consts == 4) $ applyTof a b c consts
               freeConst consts        
  where applyTof x y z [c0,c1,c2,c3] 
          = do hadamard z
               cnot y  c2
               cnot x  c0
               cnot y  c1
               cnot z  c2
               cnot c0 c3
               cnot x  c1
               cnot z  c3
               cnot c2  c0
               tgate x  
               tgate y  
               tgate z  
               tgate c0  
               tgateInv c1  
               tgateInv c2  
               tgateInv c3  
               cnot c2  c0
               cnot z  c3
               cnot x  c1
               cnot c0 c3
               cnot z  c2
               cnot y  c1
               cnot x  c0
               cnot y  c2
               hadamard z
        applyTof _ _ _ _ = assert False $ return () --Should never happen!

tofMatchedD1 :: String -> String -> String -> CircuitState () 
tofMatchedD1 e f g = do consts <- getConst 1
                        applyTof e f g (head consts)
                        freeConst consts        
  where applyTof x y z c 
          = do hadamard z
               cnot z y
               cnot x c
               cnot z x
               cnot y c
               tgateInv x 
               tgateInv y
               tgate z
               tgate c 
               cnot y c
               cnot z x
               cnot x c
               cnot z y
               hadamard z

leftTof :: String -> String -> String -> CircuitState () 
leftTof x y z 
  = do hadamard z
       cnot z y  
       cnot y x  
       tgate x
       tgateInv y
       tgate z
       cnot z y
       cnot y x
       tgateInv x
       cnot z x
       hadamard z

rightTof :: String -> String -> String -> CircuitState () 
rightTof x y z 
  = do hadamard z
       cnot z x
       tgateInv x
       cnot y x
       cnot z y
       tgate z
       tgateInv y
       tgate x
       cnot y x  
       cnot z y  
       hadamard z
