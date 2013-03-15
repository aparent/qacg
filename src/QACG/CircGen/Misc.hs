module CircGen.Misc
( applyCopy
 ,applyMux
) where

import CircUtils.Circuit


applyCopy :: [String] -> [String] -> [Gate]
applyCopy [] _          = [] 
applyCopy _ []          = [] 
applyCopy (a:as) (b:bs) = Gate "tof" [a,b] : applyCopy as bs 

applyMux :: String -> [String] -> [String] -> [Gate]
applyMux _ [] _          = [] 
applyMux _ _ []          = [] 
applyMux ctrl (a:as) (b:bs) = Gate "SWAP" [ctrl, a,b] : applyMux ctrl as bs  
