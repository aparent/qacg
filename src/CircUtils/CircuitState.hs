import CircUtils.Circuit
import Control.Monad.State
import qualified Data.Set as Set 

--(ConstInUse,ConstAvail,circ)
type CircuitState = State ([String],[String],Circuit)

getConst :: Int -> CircuitState [String]
getConst n = state $ go
  where go (constU,const,c) = (newConst, (constU ++ newConst, const, newCirc))
          where newCirc = addLines newConst c 
                availConst = Set.toList $ Set.difference  (Set.fromList const) (Set.fromList constU)
                newConst = take n availConst

freeConst :: [String] -> CircuitState ()
freeConst consts = state $ go
  where go (constU,const,c) = ( () , (newConstU, const, c) )
          where newConstU = Set.toList $ Set.difference (Set.fromList constU) (Set.fromList consts)

initLines :: [String] -> CircuitState [String]
initLines nLines = state $ go
  where go (x,y,c) = ( nLines, (x,y, Circuit nCLines  (gates c) (subcircuits c)))
          where nCLines = LineInfo  ((vars $ lInfo)++nLines) ((inputs $ lInfo)++nLines) (outputs $ lInfo) (outputLabels $ lInfo) 
                lInfo  = lineInfo c 

setOutputs :: [String] -> CircuitState ()
setOutputs outs = state $ go
  where go (x,y,c) = ( (), (x,y, Circuit nCLines  (gates c) (subcircuits c)))
          where nCLines = LineInfo  (vars $ lInfo) (inputs $ lInfo) outs (outputLabels $ lInfo) 
                lInfo  = lineInfo c 


tof :: [String] -> CircuitState () 
tof lines = state $ \(cu,c,circ) ->  (() ,(cu, c, addGates [Gate "tof" lines] circ))

--Some circuit constuctors 

mkSimpleRipple :: [String] -> [String] -> Circuit
mkSimpleRipple aLns bLns = circ
  where (_,(_,_,circ)) = runState go $ ([],(map (\x->'c':show x) [0..10]),Circuit (LineInfo [] [] [] []) [] [])
        go             = do a <- initLines aLns
                            b <- initLines bLns
                            (aOut,bOut) <- simpleRipple a b  
                            setOutputs $ aOut ++ bOut

simpleRipple :: [String] -> [String] -> CircuitState ([String], [String])
simpleRipple a b = do 
  cs <- getConst 2
  applyRipple ((cs!!0):a) b (cs!!1)
  freeConst [cs!!0]
  return (a, b ++ [(cs!!1)])
    where applyRipple (a:[]) [] z = tof [a,z] 
          applyRipple (a0:a1:as) (b0:bs) z
            = do maj a0 b0 a1 
                 applyRipple (a1:as) bs z
                 uma a0 b0 a1
          maj x y z  
            = do tof [z,y]
                 tof [z,x]
                 tof [x,y,z]
          uma x y z  
            = do tof [x,y,z]
                 tof [z,x]
                 tof [x,y]

simpleCtrlRipple :: String -> [String] -> [String] -> String -> CircuitState ([String], [String])
simpleCtrlRipple ctrl a b carry = do 
  cs <- getConst 1
  applyRipple (head cs:a) b carry
  freeConst [head cs]
  return (a, b ++ [cs!!1])
    where applyRipple (a:[]) [] z = tof [a,z] 
          applyRipple (a0:a1:as) (b0:bs) z
            = do maj a0 b0 a1 
                 applyRipple (a1:as) bs z
                 uma a0 b0 a1
          maj x y z  
            = do tof [ctrl,z,y]
                 tof [z,x]
                 tof [x,y,z]
          uma x y z  
            = do tof [x,y,z]
                 tof [z,x]
                 tof [ctrl,x,y]

mkSimpleMult :: [String] -> [String] -> Circuit
mkSimpleMult aLns bLns = circ
  where (_,(_,_,circ)) = runState go ([],map (\x->'c':show x) [0..2*(length aLns + 1)],Circuit (LineInfo [] [] [] []) [] [])
        go             = do a <- initLines aLns
                            b <- initLines bLns
                            mOut <- simpleMult a b  
                            setOutputs $ a ++ b ++ mOut


simpleMult :: [String] -> [String] -> CircuitState [String]
simpleMult a b = do 
  out <- getConst $ 2 * length a 
  start (head a) b out
  applyAdders (tail a) b (tail out) 
  return out
    where start x (y:[]) (c:cs) = tof [x,y,c]
          start x (y:ys) (c:cs) = do tof [x,y,c]
                                     start x ys cs 
          applyAdders [] b out = return ()
          applyAdders (x:xs) ys out = do simpleCtrlRipple x ys (take (length ys) out) $ out !! length ys
                                         applyAdders xs ys (tail out)
