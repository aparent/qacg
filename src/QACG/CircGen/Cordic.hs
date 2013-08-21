module QACG.CircGen.Cordic
(
  cordic
  ,mkCosSin
  ,mkLog
  ,mkSqrt
)
where 

import QACG.CircUtils.Circuit
import Control.Monad.State
import QACG.CircUtils.CircuitState
import QACG.CircGen.Add.SimpleRipple(simpleModRipple)
import QACG.CircGen.Misc(setNum)


-- K(n) = \prod_{i=0}^{n-1} 1/\sqrt{1 + 2^{-2i}} 
k :: Int -> Float
k n = product [1 / sqrt (1+2**(-2*(fromIntegral i))) | i <- [0..n]]

toBin :: Int -> [Bool]
toBin 0 = []
toBin n = (mod n 2 == 1) : toBin (div n 2)

mkLog:: Int -> [String] -> Circuit
mkLog = mkCosSin

mkSqrt:: Int -> [String] -> Circuit
mkSqrt = mkCosSin

mkCosSin:: Int -> [String] -> Circuit
mkCosSin iters beta = circ {gates = gates circ ++ reverse (gates circ)}
  where (_,(_,_,circ)) = runState go ([], ['c':show x|x<-[0::Int .. 100]] , Circuit (LineInfo [] [] [] []) [] [])
        go             = do (cosOut,sinOut) <- cordic iters beta
                            _ <- initLines beta
                            setOutputs $ cosOut ++ sinOut


-- Computes sin and cos for an angle 'beta' where -pi/2 < beta < pi/2
cordic :: Int -> [String] -> CircuitState ([String],[String])
cordic iters beta = do v1Init <- getConst $ length beta
                       v2Init <- getConst $ length beta
                       invert v1Init
                       iteration 0 v1Init v2Init beta
  where iteration :: Int -> [String] -> [String] -> [String] -> CircuitState ([String],[String])
        iteration n v1 v2 ang 
          | n < iters = do sign <- getSign ang 
                           v1C <- copy v1
                           v2C <- copy v2
                           cInvert sign v1C
                           cInvert sign v2C
                           invert v2C 
                           c1 <- getConst n
                           c2 <- getConst n
                           (_,v1') <- simpleModRipple (drop n v2C ++ c1) v1 
                           (_,v2') <- simpleModRipple (drop n v1C ++ c2) v2
                           freeConst c1
                           freeConst c2
                           ang' <- updateAng sign ang n
                           iteration (n+1) v1' v2' ang'
          | otherwise = return (v1,v2)
        getSign b = do  cs <- getConst 1
                        cnot (last b) (head cs)
                        return (head cs)  
        updateAng s a iter = do c <- getConst $ length a
                                setNum (floor $ angles (fromIntegral iter)*10^length a) c 
                                invert c
                                cInvert s c
                                (_,a') <- simpleModRipple c a 
                                cInvert s c
                                invert c
                                setNum (floor $ angles (fromIntegral iter)*10^length a) c 
                                freeConst c
                                return a'
        angles n = atan (2**(-n))
                           
invert :: [String] -> CircuitState ()
invert (x:xs) = do notgate x
                   invert xs
invert [] = return ()


cInvert :: String -> [String] -> CircuitState ()
cInvert c (x:xs) = do cnot c x
                      cInvert c xs
cInvert _ [] = return ()

copy :: [String] -> CircuitState [String]
copy x = do res <- getConst $ length x
            applyCopy x res
            return res
  where applyCopy (a:as) (b:bs) = do cnot a b 
                                     applyCopy as bs
        applyCopy _ _ = return ()

