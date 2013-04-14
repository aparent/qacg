-- | Comparison circuit from <http://arxiv.org/abs/quant-ph/0410184>
module QACG.CircGen.Comp.Ripple
( rippleComp
  ,mkLessOutOfPlace
  ,mkGreaterOutOfPlace
  ,mkLessThanOrEqualOutOfPlace
  ,mkGreaterThenOrEqualOutOfPlace
) where

import QACG.CircUtils.Circuit
import QACG.CircUtils.CircuitState
import Control.Monad.State
import Control.Exception(assert)
import Debug.Trace

import QACG.CircGen.Bit.Toffoli

-- The comparison is simply done by reverse computing an addition circuit until the carry bit is determined
-- Then undoing the computation.  If the carry bit is set then the first input is greater then the second 

rippleComp :: [String] -> [String] -> String -> CircuitState ()
rippleComp a b carry = assert (trace ("rip("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 1
  applyRippleComp (head cs:a) b carry
  freeConst [head cs]
  return ()
    where applyRippleComp (a0:[]) [] z = cnot a0 z 
          applyRippleComp (a0:a1:as) (b0:bs) z
            = do uma    a0 b0 a1 
                 applyRippleComp (a1:as) bs z
                 umaInv a0 b0 a1
          applyRippleComp _ _ _ = assert False $ return () --Should never happen!



greaterThan,greaterThanOrEqual,lessThan,lessThanOrEqual :: [String] -> [String] -> String -> CircuitState ()
greaterThan = rippleComp
greaterThanOrEqual a b c = do rippleComp b a c
                              notgate c 
lessThan a b c = rippleComp b a c
lessThanOrEqual a b c = do rippleComp a b c 
                           notgate c

mkLessOutOfPlace,mkGreaterOutOfPlace,mkLessThanOrEqualOutOfPlace,mkGreaterThenOrEqualOutOfPlace :: [String] -> [String] -> String -> Circuit
mkLessOutOfPlace = mkComp lessThan
mkGreaterOutOfPlace = mkComp greaterThan 
mkLessThanOrEqualOutOfPlace = mkComp lessThanOrEqual 
mkGreaterThenOrEqualOutOfPlace = mkComp greaterThanOrEqual

mkComp :: ([String] -> [String] -> String -> CircuitState ()) -> [String] -> [String] -> String -> Circuit
mkComp comp aLns bLns carry = circ
  where (_,(_,_,circ)) = runState go ([], ['c':show x|x<-[0..10]] , Circuit (LineInfo [] [] [] []) [] [])
        go             = do comp aLns bLns carry
                            _ <- initLines aLns
                            _ <- initLines bLns
                            _ <- initLines [carry]
                            setOutputs $ aLns ++ bLns ++ [carry]

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
