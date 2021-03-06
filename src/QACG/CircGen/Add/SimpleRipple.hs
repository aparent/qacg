module QACG.CircGen.Add.SimpleRipple
( simpleRipple
 ,simpleModRipple
 ,simpleCtrlRipple
 ,mkSimpleRipple
 ,mkSimpleModRipple
 ,mkSimpleSubtract
 ,mkSimpleCtrlRipple
 ,simpleSubtract
 ,simpleCtrlSub
) where

import QACG.CircUtils.Circuit
import QACG.CircUtils.CircuitState
import Control.Monad.State
import Control.Exception(assert)
import Debug.Trace

import QACG.CircGen.Bit.Toffoli

-- |Generates the addition circuit in <http://arxiv.org/abs/quant-ph/0410184> and returns it as a circuit
mkSimpleRipple :: [String] -> [String] -> String -> Circuit
mkSimpleRipple aLns bLns carry = circ
  where (_,(_,_,circ)) = runState go ([], ['c':show x|x<-[0::Int .. 10]] , Circuit (LineInfo [] [] [] []) [] [])
        go             = do (aOut,bOut) <- simpleRipple aLns bLns carry
                            _ <- initLines aLns
                            _ <- initLines bLns
                            _ <- initLines [carry]
                            setOutputs $ aOut ++ bOut ++ [carry]

mkSimpleModRipple :: [String] -> [String] -> Circuit
mkSimpleModRipple aLns bLns = circ
  where (_,(_,_,circ)) = runState go ([], ['c':show x|x<-[0::Int .. 10]] , Circuit (LineInfo [] [] [] []) [] [])
        go             = do (aOut,bOut) <- simpleModRipple aLns bLns
                            _ <- initLines aLns
                            _ <- initLines bLns
                            setOutputs $ aOut ++ bOut


mkSimpleSubtract :: [String] -> [String] -> Circuit
mkSimpleSubtract aLns bLns = circ
  where (_,(_,_,circ)) = runState go ([], ['c':show x|x<-[0::Int .. 10]] , Circuit (LineInfo [] [] [] []) [] [])
        go             = do (aOut,bOut) <- simpleSubtract aLns bLns 
                            _ <- initLines aLns
                            _ <- initLines bLns
                            setOutputs $ aOut ++ bOut


mkSimpleCtrlRipple :: String -> [String] -> [String] -> String -> Circuit
mkSimpleCtrlRipple ctrl aLns bLns carry = circ
  where (_,(_,_,circ)) = runState go ([], ['c':show x|x<-[0::Int .. 10]] , Circuit (LineInfo [] [] [] []) [] [])
        go             = do (aOut,bOut) <- simpleCtrlRipple ctrl aLns bLns carry
                            _ <- initLines aLns
                            _ <- initLines bLns
                            _ <- initLines [carry]
                            _ <- initLines [ctrl]
                            setOutputs $ aOut ++ bOut ++ [carry]


-- |Used to pad inputs that need to be equal length.  
-- In the case of these adders the a input is returned and the result is stored in b.
-- This means that padding bits may be freed in the case where they are added to a but not b
padInputs :: [String] -> [String] -> ([String]->[String]->CircuitState a) -> CircuitState (([String],[String]) , a)
padInputs a b f 
  | length a > length b  = do padding <- getConst $ length a - length b  
                              r <- f a (b++padding)         
                              return ((a,b),r)
  | length a < length b  = do padding <- getConst $ length b - length a
                              r <- f (a++padding) b 
                              freeConst padding
                              return ((a,b),r)
  | otherwise            = do r <- f a b 
                              return ((a,b),r)



maj :: String -> String -> String -> CircuitState () 
maj x y z 
  = do cnot z y
       cnot z x
       leftTof x y z

uma :: String -> String -> String -> CircuitState () 
uma x y z  
  = do rightTof x y z
       cnot z x
       cnot x y

simpleModRipple :: [String] -> [String] -> CircuitState ([String], [String])
simpleModRipple a b = assert (trace ("rip("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 1
  applyRipple (head cs:a) b
  freeConst [head cs]
  return (a, b)
    where applyRipple (a0:a1:[]) (b0:[]) 
            = do cnot a1 b0 
                 cnot a0 b0
          applyRipple (a0:a1:as) (b0:bs) 
            = do maj a0 b0 a1 
                 applyRipple (a1:as) bs 
                 uma a0 b0 a1
          applyRipple _ _ = assert False $ return () --Should never happen!

simpleRipple :: [String] -> [String] -> String -> CircuitState ([String], [String])
simpleRipple a b carry = assert (trace ("rip("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 1
  applyRipple (head cs:a) b carry
  freeConst [head cs]
  return (a, b ++ [carry])
    where applyRipple (a0:[]) [] z = cnot a0 z 
          applyRipple (a0:a1:as) (b0:bs) z
            = do maj a0 b0 a1 
                 applyRipple (a1:as) bs z
                 uma a0 b0 a1
          applyRipple _ _ _ = assert False $ return () --Should never happen!

simpleSubtract :: [String] -> [String] -> CircuitState ([String], [String])
simpleSubtract a b = assert (trace ("sub("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 2
  applyRippleSub (head cs:a) b (cs!!1)
  freeConst [head cs]
  return (a, b ++ [cs!!1])
    where applyRippleSub (a0:[]) [] z = cnot a0 z 
          applyRippleSub (a0:a1:as) (b0:bs) z
            = do uma a0 b0 a1 
                 applyRippleSub (a1:as) bs z
                 maj a0 b0 a1
          applyRippleSub _ _ _ = assert False $ return () --Should never happen!

simpleCtrlRipple :: String -> [String] -> [String] -> String -> CircuitState ([String], [String])
simpleCtrlRipple ctrl a b carry = assert (trace ("RipCon("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 1
  applyRippleC (head cs:a) b carry
  freeConst [head cs]
  return (a, b++[carry])
    where applyRippleC (a0:[]) [] z = cnot a0 z 
          applyRippleC (a0:a1:as) (b0:bs) z
            = do majC a0 b0 a1 
                 applyRippleC (a1:as) bs z
                 umaC a0 b0 a1
          applyRippleC _ _ _ = assert False $ return () --Should never happen!
          majC x y z  
            = do tof ctrl z y
                 cnot z x
                 tof x y z
          umaC x y z  
            = do tof x y z
                 cnot z x
                 tof ctrl x y

simpleCtrlSub :: String -> [String] -> [String] -> String -> CircuitState ([String], [String])
simpleCtrlSub ctrl a b carry = assert (trace ("RipCon("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 1
  applyRippleC (head cs:a) b carry
  freeConst [head cs]
  return (a, b )
    where applyRippleC (a0:[]) [] z = cnot a0 z 
          applyRippleC (a0:a1:as) (b0:bs) z
            = do umaC a0 b0 a1 
                 applyRippleC (a1:as) bs z
                 majC a0 b0 a1
          applyRippleC _ _ _ = assert False $ return () --Should never happen!
          majC x y z  
            = do tof ctrl z y
                 cnot z x
                 tof x y z
          umaC x y z  
            = do tof x y z
                 cnot z x
                 tof ctrl x y
