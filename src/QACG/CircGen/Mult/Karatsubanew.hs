module CircGen.Mult.Karatsubanew
( karatsuba
) where

import CircUtils.Circuit
import CircGen.Mult.SimpleMultAlt
import CircGen.Add.SimpleRipple
import Data.List

karatsuba :: Int -> Int -> Circuit 
karatsuba size cuttoff = Circuit (LineInfo vars inputs outputs outputLabs) kGates subs
  where subs = subcircs size cuttoff
        (x0,x1,y0,y1) = inputVars size
        inputs = x0 ++ x1 ++ y0 ++ y1
        vars = inputs ++ ancres size ++ (allAncilla size cuttoff)
        outputs = inputs ++ ancres size
        outputLabs = inputs ++ ancres size
        kGates = karaGates size cuttoff

subcircs :: Int -> Int -> [(Circuit,String)]
subcircs n c = karaSubs ++ multSubs ++ adderSubs 
  where karaSubs = map (\x -> (kara x c, "KARA" ++ show x)) $ karas n c
        multSubs = map (\x -> (simpleMultAlt x, "MULT" ++ show x)) $ mults n c
        adderSubs = map (\x -> (simpleRipple x, "ADD" ++ show x)) $ adders n c

-- | Take a size and a cuttoff return a circuit 
kara :: Int -> Int -> Circuit
kara n c = Circuit (LineInfo vars [] [] []) kGates []
  where (x0,x1,y0,y1) = inputVars n
        inputs = x0 ++ x1 ++ y0 ++ y1
        vars = inputs ++ ancres n ++ allAncilla n c
        kGates = karaGates n c

round2Up x = div x 2 + mod x 2

karaGates :: Int -> Int -> [Gate]
karaGates n c = adderx ++ addery ++ kGates ++ adderx ++ addery
  where kGates  = [kGateZ0,kGateZ1,kGateZ2]
        kGateZ0 = kGate (round2Up n) c (x0++x1++(ancz0 n))
        kGateZ1 = kGate (round2Up n + 1) c (ancxx n++ancyy n++(ancres n))
        kGateZ2 = kGate (round2Up n) c (y0++y1++(ancz2 n))
        adderx = copyDown x0 (ancxx n) ++ [adder x1 (ancxx n)]
        addery = copyDown y0 (ancyy n) ++ [adder y1 (ancyy n)]
        (x0,x1,y0,y1) = inputVars n
        adder a b = Gate ("ADD" ++ (show $ length a)) $ (combLists b a) -- add ancilla!

kGate :: Int -> Int -> [String] -> Gate
kGate n c s | n > c = Gate ("KARA" ++ show n) s
            | otherwise = Gate ("MULT" ++ show n) s 


copyDown :: [String] -> [String] -> [Gate]
copyDown (a:as) (b:bs) = Gate "tof" [a,b] : copyDown as bs
copyDown [] _ = []

combLists :: [x] -> [x] -> [x]
combLists a b = concat $ zipWith (\x y -> [x,y]) a b

-- | (x0,x1,y0,y1)
inputVars :: Int -> ([String],[String],[String],[String])
inputVars n =    (x0,x1,y0,y1)
  where x0 = map (\x -> 'x' : show x) [0 .. div n 2 - 1] 
        x1 = map (\x -> 'x' : show x) [div n 2 .. n - 1]  
        y0 = map (\x -> 'y' : show x) [0 .. div n 2 - 1]
        y1 = map (\x -> 'y' : show x) [div n 2 .. n - 1] 

-- | sizes of karatsuba circuits needed.  n:size, c:cuttoff
karas :: Int -> Int -> [Int]
karas n c = sort $ filter (>c) $ nub $ karaList n
  where karaList n | n < 4 = []
                   | otherwise = round2Up n : (round2Up n) +1 : (karaList $ round2Up n) ++ (karaList $ (round2Up n)+1) 
-- | sizes of multiplication circuits needed 
mults :: Int -> Int -> [Int]
--mults n c = baseSize : baseSize + 1 : []
--  where baseSize = div (head $ karas n c) 2
mults _ c = [4..c]

-- | sizes of addition circuits needed
adders :: Int -> Int -> [Int]
adders n c = sort $ nub $ concatMap (\x -> [div x 2, x]) $ n : karas n c

-- | this function finds all the ancilla needed by all calls of kara 
allAncilla :: Int -> Int -> [String]
allAncilla n c = ancilla n ++ (concatMap ancilla $ karas n c)

--kara of size n has i
ancilla :: Int -> [String]
ancilla n = ancxx n ++ ancyy n ++ ancz0 n ++ ancz2 n ++ anccons n

anc str n num = map (\x -> ('k':show n) ++ str ++ show x) [ 0 .. num ]
ancxx n = anc "xx" n $ div n 2 + 1
ancyy n = anc "yy" n $ div n 2 + 1
ancres n = anc "res" n $ 2*n
ancz0 n = anc "z0" n n
ancz1 n = anc "z1" n $ n+2
ancz2 n = anc "z2" n n
anccons n = anc "c" n 3
