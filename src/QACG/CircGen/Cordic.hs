module QACG.CircGen.Cordic
(
cordic
)
where 

import QACG.CircUtils.CircuitState

import QACG.CircGen.Add.SimpleRipple(simpleRipple)
import QACG.CircGen.Mult.SimpleMult(simpleMult)
import QACG.CircGen.Bit.MCToff(mctToff)
import QACG.CircGen.Misc(setNum)


-- For lookup table
angles = [atan (2**(-x)) | x <- [0..27]]

-- K(n) = \prod_{i=0}^{n-1} 1/\sqrt{1 + 2^{-2i}} 
k n = product [1 / sqrt (1+2**(-2*i))| i <- [0..n]]


toBin :: Int -> [Bool]
toBin 0 = []
toBin n = (mod n 2 == 1) : (toBin $ div n 2)

lookup :: [String] -> [String] -> [Int] -> CircuitState ()
lookup inputs outputs table = applyTable 0 binTable inputs outputs 
  where binTable = map toBin table
        applyTable n (t:ts) = do applyToff n t
                                 applyTable (n+1) ts 
        applyToff n targs = do setNum n inputs
                               mctToff inputs outputs
                               setNum n inputs 
          where out = snd $ unzip $ filter (\x-> fst x) $ zip targs outputs

-- Computes sin and cos for an angle 'beta' where -pi/2 < beta < pi/2
cordic :: Int -> [String] -> CircuitState ([String],[String])
cordic n beta = iteration 0 ["a1","a2","a3","a4"] ["b1","b2","b3","b4"] ["z1","z2","z3","z4"]
  where size = length beta
        iteration n v1 v2 ang = do sign <- getSign ang 
                                   v1C <- getConst $ size v1
                                   v2C <- getConst $ size v2
                                   cInvert sign v1C
                                   cInvert sign v2C
                                   invert v2C 
                                   c1 <- getConst n
                                   c2 <- getConst n
                                   adderConsts <- getConst 2
                                   simpleRipple ((drop n v2C) ++ c1) v1 (head adderConsts)
                                   simpleRipple ((drop n v1C) ++ c2) v2 (head $ tail adderConsts)
                                   freeConst c1
                                   freeConst c2 
        getSign b = do  cs <- getConst 1
                        cnot (last b) (head cs)
                        return (head cs)  
                           
invert :: [String] -> CircuitState ()
invert (x:xs) = do notgate x
                   invert xs
invert [] = return ()


cInvert :: String -> [String] -> CircuitState ()
cInvert c (x:xs) = do cnot c x
                      cInvert c xs
cInvert _ [] = return ()

copy :: [String] -> CircuitState ([String])
copy x = do res <- getConst $ length x
            applyCopy x res
            return res
  where applyCopy (a:as) (b:bs) = do cnot a b 
                                     applyCopy as bs
        applyCopy _ _ = return ()

