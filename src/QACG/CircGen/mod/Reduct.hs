module QACG.CircGen.Comp.Reduct
( modAddReduct
)
where

import QACG.CircUtils.Circuit
import QACG.CircUtils.CircuitState
import Control.Monad.State
import Control.Exception(assert)
import Debug.Trace

import QACG.CircGen.Comp.Ripple
import QACG.CircGen.Add.SimpleRipple(simpleCtrlSub)
import QACG.CircGen.Mult.SimpleMult(simpleMult)

modAddReduct :: Int -> [String] -> CircuitState (String)  
modAddReduct N a = do NBits <- getConst $ ceiling $ logBase 2 N
                      anc <- head $ getConst 1
                      setNum N NBits
                      rippleComp NBits a anc
                      simpleCtrlSub anc NBits a
                      setNum N NBits
                      freeConst NBits 
                      return anc

setNum :: Int -> [String] -> CircuitState () 
setNum N [] = return () 
setNum N (a:as) 
  | N `mod` 2 == 1 = do not a 
                        setNum (N `div` 2) as
  | otherwise      = setNum (N `div` 2) as 

ctrlSetNum :: Int -> [String] -> String -> CircuitState () --Sets the num if control bit is high or else sets 1 
ctrlSetNum N [] = return () 
ctrlSetNum num bits ctrl = do not ctrl 
                              cnot ctrl (head bits) 
                              not ctrl
                              ctrlSetNum' 
  where ctrlSetNum' n (b:bs) c 
          | n `mod` 2 == 1 = do cnot c b 
                                ctrlSetNum' (n `div` 2) bs c
          | otherwise      =    ctrlSetNum  (n `div` 2) bs c



add :: Int -> [String] -> [String] -> CircuitState (String)
add N a b = do anc <- head $ getConst 1
               simpleRipple a b anc
               garb <- modAddReduct N (b++[anc]) --This creates one garbage bit
               freeConst anc

ctrlAddConst :: Int -> [String] -> String -> CircuitState ([String]) 
ctrlAddConst N a ctrl = do NBits <- getConst sizeN
                           setNum N NBits
                           (_, res) <- simpleCtrlRipple ctrl NBits (init a) (last a) 
                           setNum N NBits
                           freeConst NBits
                           return res
  where sizeN = ceiling $ logBase 2 N

ctrlMultConst :: Int -> [String] -> String -> CircuitState ([String]) 
ctrlMultConst N a ctrl = do NBits <- getConst sizeN
                                     ctrlSetNum N NBits ctrl
                                     out <- simpleMult a NBits 
                                     ctrlSetNum N NBits ctrl
                                     freeConst NBits
                                     return out
  where sizeN = ceiling $ logBase 2 N

divConst :: [String] -> Int -> CircuitState ([String],[String])
divConst a N
  | length a < sizeN = return ([], a)
  | otherwise = do quotBit <- modAddReduct (N * 2^(length a - sizeN)) 
                   freeConst (head a)
                   (quot,remain) <- divConst (tail a) n
                   return ((quotBit:quot),remain)
  where sizeN = ceiling $ logBase 2 N

expon :: [String] -> Int -> Int -> CircuitState ([String]) --Mod Exponentiation with known base and mod
expon a base N = do state = inital <- getConst ceiling $ logBase 2 base
                            setNum 1 inital
                            res <- expon' inital a 1
  where  expon' curr [] mults = return ()
         expon' curr (c:cs) mults = do new <- expon'' curr c mults 
                                       res <- expon' new cs (mults*2) 
                                       return res
    where expon'' curr c n =  do new <- ctrlMultConst base curr c
                                 (_,reduced) <- divConst new N
                                 res <- expon'' reduced c (n - 1)
                                 return res
          expon'' curr _ 0 =  return 0
