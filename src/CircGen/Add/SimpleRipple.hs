module CircGen.Add.SimpleRipple
( simpleRipple
 ,simpleCtrlRipple
 ,mkSimpleRipple
 ,mkSimpleCtrlRipple
) where

import CircUtils.Circuit
import CircUtils.CircuitState
import Control.Monad.State
import Control.Exception(assert)
import Debug.Trace

import CircGen.Bit.Toffoli

-- |Generates the addition circuit in <http://arxiv.org/abs/quant-ph/0410184> and returns it as a circuit
mkSimpleRipple :: [String] -> [String] -> String -> Circuit
mkSimpleRipple aLns bLns carry = circ
  where (_,(_,_,circ)) = runState go ([], map (\x->'c':show x) [0..10] , Circuit (LineInfo [] [] [] []) [] [])
        go             = do (aOut,bOut) <- simpleRipple aLns bLns carry
                            initLines aLns
                            initLines bLns
                            initLines [carry]
                            setOutputs $ aOut ++ bOut ++ [carry]

mkSimpleCtrlRipple :: String -> [String] -> [String] -> String -> Circuit
mkSimpleCtrlRipple ctrl aLns bLns carry = circ
  where (_,(_,_,circ)) = runState go ([], map (\x->'c':show x) [0..10] , Circuit (LineInfo [] [] [] []) [] [])
        go             = do (aOut,bOut) <- simpleCtrlRipple ctrl aLns bLns carry
                            initLines aLns
                            initLines bLns
                            initLines [carry]
                            setOutputs $ aOut ++ bOut ++ [carry]


maj x y z 
  = do cnot z y
       cnot z x
       leftTof x y z

uma x y z  
  = do rightTof x y z
       cnot z x
       cnot x y

simpleRipple :: [String] -> [String] -> String -> CircuitState ([String], [String])
simpleRipple a b carry = assert (trace ("rip("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 1
  applyRipple (head cs:a) b carry
  freeConst [head cs]
  return (a, b ++ [carry])
    where applyRipple (a:[]) [] z = cnot a z 
          applyRipple (a0:a1:as) (b0:bs) z
            = do maj a0 b0 a1 
                 applyRipple (a1:as) bs z
                 uma a0 b0 a1

simpleSubtract :: [String] -> [String] -> CircuitState ([String], [String])
simpleSubtract a b = assert (trace ("sub("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 2
  applyRipple (head cs:a) b (cs!!1)
  freeConst [head cs]
  return (a, b ++ [cs!!1])
    where applyRipple (a:[]) [] z = cnot a z 
          applyRipple (a0:a1:as) (b0:bs) z
            = do uma a0 b0 a1 
                 applyRipple (a1:as) bs z
                 maj a0 b0 a1

simpleCtrlRipple :: String -> [String] -> [String] -> String -> CircuitState ([String], [String])
simpleCtrlRipple ctrl a b carry = assert (trace ("RipCon("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 1
  applyRippleC (head cs:a) b carry
  freeConst [head cs]
  return (a, b )
    where applyRippleC (a:[]) [] z = cnot a z 
          applyRippleC (a0:a1:as) (b0:bs) z
            = do majC a0 b0 a1 
                 applyRippleC (a1:as) bs z
                 umaC a0 b0 a1
          majC x y z  
            = do tof ctrl z y
                 cnot z x
                 tof x y z
          umaC x y z  
            = do tof x y z
                 cnot z x
                 tof ctrl x y
