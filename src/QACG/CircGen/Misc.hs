module CircGen.Misc
( applyCopy
 ,applyMux
 ,setNum
) where

import CircUtils.Circuit
import QACG.CircUtils.CircuitState

setNum :: Int -> [String] -> CircuitState () 
setNum N [] = return () 
setNum N (a:as) 
  | N `mod` 2 == 1 = do notgate a 
                        setNum (N `div` 2) as
  | otherwise      = setNum (N `div` 2) as 



applyCopy :: [String] -> [String] -> [Gate]
applyCopy [] _          = [] 
applyCopy _ []          = [] 
applyCopy (a:as) (b:bs) = Gate "tof" [a,b] : applyCopy as bs 

applyMux :: String -> [String] -> [String] -> [Gate]
applyMux _ [] _          = [] 
applyMux _ _ []          = [] 
applyMux ctrl (a:as) (b:bs) = Gate "SWAP" [ctrl, a,b] : applyMux ctrl as bs  

