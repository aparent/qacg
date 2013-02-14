import CircUtils.Circuit
import Control.Monad.State

--(ConstInUse,ConstAvail,circ)
type CircuitState = State ([String],[String],Circuit)

getConst :: Int -> CircuitState [String]
getConst n = state $ go
  where go (constU,constA,c) = 
          where nConst = take n ConstA
                nConstA = drop n ConstA
                nConstU = drop n ConstA
-- Uhhhh consider map??1

initLines :: [String] -> CircuitState [String]
initLines nLines = state $ go
  where go (x,y,c) = ( nLines, (x,y, Circuit nCLines  (gates c) (subcircuits c)))
          where nCLines = LineInfo  ((vars $ lInfo)++nLines) ((inputs $ lInfo)++nLines) (outputs $ lInfo) (outputLabels $ lInfo) 
                lInfo  = lineInfo c   
