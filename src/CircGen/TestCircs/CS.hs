module CircGen.TestCircs.CS
( cs
) where

import CircUtils.Circuit
import CircGen.Add.SimpleRipple
import CircGen.Misc

cs :: Circuit 
cs = Circuit csLines csGates []
  where csLines = LineInfo clines clines clines clines

csGates :: [Gate] 
csGates = copies ++ adders ++ applyMux "ctrl" ylines yclines
 where adders = combineLists (applySimpleRipple xlines  ylines  "z0" "c0") (applySimpleRipple xclines yclines "z1" "c1")
       copies = combineLists (applyCopy xlines xclines) (applyCopy ylines yclines)

clines :: [String]
clines = ["ctrl"] ++ xlines ++ ["c0"] ++ ylines ++ ["z0"] ++ xclines ++ ["c1"] ++ yclines ++ ["z1"]


xlines  = ["x0" ,"x1" ,"x2" ,"x3" ]
ylines  = ["y0" ,"y1" ,"y2" ,"y3" ]
xclines = ["xc0","xc1","xc2","xc3"]
yclines = ["yc0","yc1","yc2","yc3"]

combineLists :: [a] -> [a] -> [a]
combineLists [] [] = []
combineLists x  [] = x
combineLists []  y = y
combineLists (x:xs) (y:ys) = x : y : combineLists xs ys
