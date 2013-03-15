module CircGen.Bit.Compare
( lessThen
) where


import CircGen.Bit.Toffoli

lessThen :: [String] -> [String] -> String -> CircuitState ([String], [String])
lessThen a b t = assert (trace ("rip("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 1
  applyComp (head cs:a) b t
  freeConst [head cs]
  return (a, b ++ [t])
    where applyComp (a:[]) [] z = cnot a z 
          applyComp (a0:a1:as) (b0:bs) z
            = do rightTof x y z
                 cnot z x
                 cnot x y
                 applyComp (a1:as) bs z
                 cnot x y
                 cnot z x
                 rightTof x y z
