-- | Comparison circuit from <http://arxiv.org/abs/quant-ph/0410184>
module QACG.CircGen.Comp.Ripple
( rippleComp
  ,mkRippleComp
) where

import QACG.CircUtils.Circuit
import QACG.CircUtils.CircuitState
import Control.Monad.State
import Control.Exception(assert)
import Debug.Trace

import QACG.CircGen.Bit.Toffoli

-- The comparison is simply done by reverse computing an addition circuit until the carry bit is determined
-- Then undoing the computation.  If the carry bit is set then the first input is greater then the second 

rippleComp :: [String] -> [String] -> String -> CircuitState ([String], [String])
rippleComp a b carry = assert (trace ("rip("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 1
  applyRippleComp (head cs:a) b carry
  freeConst [head cs]
  return (a, b ++ [carry])
    where applyRippleComp (a0:[]) [] z = cnot a0 z 
          applyRippleComp (a0:a1:as) (b0:bs) z
            = do uma    a0 b0 a1 
                 applyRippleComp (a1:as) bs z
                 umaInv a0 b0 a1
          applyRippleComp _ _ _ = assert False $ return () --Should never happen!

mkRippleComp :: [String] -> [String] -> String -> Circuit
mkRippleComp aLns bLns carry = circ
  where (_,(_,_,circ)) = runState go ([], ['c':show x|x<-[0..10]] , Circuit (LineInfo [] [] [] []) [] [])
        go             = do (aOut,bOut) <- rippleComp aLns bLns carry
                            _ <- initLines aLns
                            _ <- initLines bLns
                            _ <- initLines [carry]
                            setOutputs $ aOut ++ bOut ++ [carry]

uma :: String -> String -> String -> CircuitState () 
uma x y z  
  = do rightTof x y z
       cnot z x
       cnot x y

umaInv :: String -> String -> String -> CircuitState () 
umaInv x y z  
  = do cnot x y
       cnot z x
       rightTof x y z
