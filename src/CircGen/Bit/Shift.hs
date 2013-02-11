module CircGen.Bit.Shift
( contShift
  ,applyContShift
) where

import CircUtils.Circuit

contShift :: Int -> Circuit 
contShift n = Circuit shiftLines (applyContShift "ctrl" $ tail slines) []
  where slines ="ctrl" : map (\x -> 'x':show x) [0..(n-1)]
        shiftLines = LineInfo slines slines slines slines


applyContShift :: String -> [String] -> [Gate]
applyContShift _ []         = []
applyContShift _ (_:[])     = [] 
applyContShift ctrl (x0:x1:xs) =   Gate "tof" [x0,x1] 
                                 : Gate "tof" [ctrl,x1,x0]
                                 : Gate "tof" [x0,x1] 
                                 : applyContShift ctrl (x1:xs)
