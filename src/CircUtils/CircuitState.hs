module CircUtils.CircuitState 
(
  CircuitState
  ,getConst
  ,freeConst
  ,initLines 
  ,setOutputs
  ,cnot
  ,notgate
  ,tgate
  ,tgateInv
  ,hadamard
) where


import CircUtils.Circuit
import Control.Monad.State

--For karatsuba stuff
import Data.List(minimumBy,(\\)) 
import Data.Function(on)
import Control.Exception(assert)

import Debug.Trace
sTrace a = trace (show a) a 

--(ConstInUse,ConstAvail,circ)
type CircuitState = State ([String],[String],Circuit)

getConst :: Int -> CircuitState [String]
getConst n = state go
  where go (constU,const,c) = (newConst, (constU ++ newConst, const, newCirc))
          where newCirc = addLines newConst c 
                availConst = const \\ constU
                newConst = take n availConst

freeConst :: [String] -> CircuitState ()
freeConst consts = state go
  where go (constU,const,c) = ( () , (constU \\ consts, const, c) )

initLines :: [String] -> CircuitState [String]
initLines nLines = state go
  where go (x,y,c) = ( nLines, (x,y, Circuit nCLines  (gates c) (subcircuits c)))
          where nCLines = LineInfo  (vars lInfo ++ nLines) (inputs lInfo ++ nLines) (outputs lInfo) (outputLabels lInfo) 
                lInfo  = lineInfo c 

setOutputs :: [String] -> CircuitState ()
setOutputs outs = state go
  where go (x,y,c) = ( (), (x,y, Circuit nCLines  (gates c) (subcircuits c)))
          where nCLines = LineInfo  (vars lInfo) (inputs lInfo) outs (outputLabels lInfo) 
                lInfo  = lineInfo c 

appendGate :: String -> [String] -> CircuitState ()
appendGate gateName lines = state $ \(cu,c,circ) ->  (() ,(cu, c, addGates [Gate gateName lines] circ))


hadamard :: String -> CircuitState ()
hadamard x = appendGate "H" [x]

tgate :: String -> CircuitState () 
tgate x = appendGate "T" [x] 

tgateInv :: String -> CircuitState () 
tgateInv x = appendGate "T*" [x] 

cnot :: String -> String -> CircuitState ()
cnot c t = appendGate "tof" [c,t]

notgate :: String -> CircuitState ()
notgate x = appendGate "tof" [x]
