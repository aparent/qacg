module CircGen.Mult.SimpleMult
( simpleMult
  ,mkSimpleMult
) where


import CircUtils.Circuit
import CircUtils.CircuitState
import Control.Monad.State

import CircGen.Add.SimpleRipple
import CircGen.Bit.Toffoli

import Control.Exception(assert)
-- | Generates a simple addition based multiplication circuit 
mkSimpleMult :: [String] -> [String] -> Circuit
mkSimpleMult aLns bLns = circ
  where (_,(_,_,circ)) = runState go ([], ['c':show x|x<-[0..3*(length aLns + 1)]],Circuit (LineInfo [] [] [] []) [] [])
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
    where start x (y:[]) (c:_) = tof x y c
          start x (y:ys) (c:cs) = do tof x y c
                                     start x ys cs 
          start _ _ _ = assert False $ return () --Should never happen!
          applyAdders [] _ _ = return ()
          applyAdders (x:xs) ys out = do _ <- simpleCtrlRipple x ys (take (length ys) out) $ out !! length ys
                                         applyAdders xs ys (tail out)
