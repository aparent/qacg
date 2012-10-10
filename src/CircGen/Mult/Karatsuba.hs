

karatsuba :: [String] -> [String] -> String
karatsuba (x:[]) (y:[]) =  "(" ++ x ++ "." ++ y ++ ")"
karatsuba (x0:x1:[]) (y:[]) =  "(" ++ x0 ++ x1 ++ "." ++ y ++ ")"
karatsuba (x:[]) (y0:y1:[]) =  "(" ++ x ++ "." ++ y0 ++ y1 ++ ")"
karatsuba x y = "(" ++ z2 ++")" ++ "<<" ++ show (len*2) ++ " + " ++ "(" ++ z1 ++")" ++ "<<" ++ show (len) ++ " + " ++ "(" ++ z0 ++ ")"
  where len     = length x
        (x0,x1) = splitAt (quot len 2) x
        (y0,y1) = splitAt (quot len 2) y
        z0      = karatsuba x0 y0
        z1      = karatsuba x1 y0  ++ " + " ++ karatsuba x0 y0
        z2      = karatsuba x1 y1

xlist a = take a (zipWith (++) (repeat "x") $ map show [0::Int ..])
ylist a = take a (zipWith (++) (repeat "y") $ map show [0::Int ..])

testKara n = length (karatsuba (xlist n) (ylist n))

test n = zip (map show [1..n]) (map (show.testKara) [1..n])

csv [] = ""    
csv ((x,y):xs) = x ++ "," ++ y ++ "\n" ++ csv xs     
