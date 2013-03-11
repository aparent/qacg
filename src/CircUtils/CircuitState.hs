module CircUtils.CircuitState 
(
  CircuitState
  ,getConst
  ,freeConst
  ,initLines 
  ,setOutputs
  ,tof
  ,leftTof
  ,rightTof
  ,cnot
  ,notgate
) where

import CircUtils.Circuit
import Control.Monad.State

--For karatsuba stuff
import Data.List(minimumBy,(\\)) 
import Data.Function(on)
import Control.Exception(assert)
import qualified Data.MemoCombinators as Memo


import Debug.Trace

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
  where go (constU,const,c) = ( () , (newConstU, const, c) )
          where newConstU = constU \\ consts

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

--tof :: String -> String -> String -> CircuitState () 
--tof x y z = appendGate "tof" [x,y,z]

tof :: String -> String -> String -> CircuitState () 
tof x y z = do consts <- getConst 4
               assert (length (sTrace consts) == 4) $ applyTof x y z consts
               freeConst consts        
  where applyTof x y z [c0,c1,c2,c3] 
          = do hadamard z
               cnot y  c2
               cnot x  c0
               cnot y  c1
               cnot z  c2
               cnot c0 c3
               cnot x  c1
               cnot z  c3
               cnot c2  c0
               tgate x  
               tgate y  
               tgate z  
               tgate c0  
               tgateInv c1  
               tgateInv c2  
               tgateInv c3  
               cnot c2  c0
               cnot z  c3
               cnot x  c1
               cnot c0 c3
               cnot z  c2
               cnot y  c1
               cnot x  c0
               cnot y  c2
               hadamard z

tofMatchedD1 :: String -> String -> String -> CircuitState () 
tofMatchedD1 x y z = do consts <- getConst 1
                        applyTof x y z (head consts)
                        freeConst consts        
  where applyTof x y z c 
          = do hadamard z
               cnot z y
               cnot x c
               cnot z x
               cnot y c
               tgateInv x 
               tgateInv y
               tgate z
               tgate c 
               cnot y c
               cnot z x
               cnot x c
               cnot z y
               hadamard z

leftTof :: String -> String -> String -> CircuitState () 
leftTof x y z 
  = do hadamard z
       cnot z y  
       cnot y x  
       tgate x
       tgateInv y
       tgate z
       cnot z y
       cnot y x
       tgateInv x
       cnot z x
       hadamard z

rightTof :: String -> String -> String -> CircuitState () 
rightTof x y z 
  = do hadamard z
       cnot z x
       tgateInv x
       cnot y x
       cnot z y
       tgate z
       tgateInv y
       tgate x
       cnot y x  
       cnot z y  
       hadamard z


--Some circuit constuctors 

mkSimpleRipple :: [String] -> [String] -> String -> Circuit
mkSimpleRipple aLns bLns carry = circ
  where (_,(_,_,circ)) = runState go ([], map (\x->'c':show x) [0..10] , Circuit (LineInfo [] [] [] []) [] [])
        go             = do (aOut,bOut) <- simpleRipple aLns bLns carry
                            initLines aLns
                            initLines bLns
                            initLines [carry]
                            setOutputs $ aOut ++ bOut ++ [carry]

maj x y z 
  = do cnot z y
       cnot z x
       leftTof x y z

uma x y z  
  = do rightTof x y z
       cnot z x
       cnot x y

simpleRipple :: [String] -> [String] -> String -> CircuitState ([String], [String])
simpleRipple a b carry = assert (trace ("rip("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 1
  applyRipple (head cs:a) b carry
  freeConst [head cs]
  return (a, b ++ [carry])
    where applyRipple (a:[]) [] z = cnot a z 
          applyRipple (a0:a1:as) (b0:bs) z
            = do maj a0 b0 a1 
                 applyRipple (a1:as) bs z
                 uma a0 b0 a1

simpleSubtract :: [String] -> [String] -> CircuitState ([String], [String])
simpleSubtract a b = assert (trace ("sub("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 2
  applyRipple (head cs:a) b (cs!!1)
  freeConst [head cs]
  return (a, b ++ [cs!!1])
    where applyRipple (a:[]) [] z = cnot a z 
          applyRipple (a0:a1:as) (b0:bs) z
            = do uma a0 b0 a1 
                 applyRipple (a1:as) bs z
                 maj a0 b0 a1

simpleCtrlRipple :: String -> [String] -> [String] -> String -> CircuitState ([String], [String])
simpleCtrlRipple ctrl a b carry = assert (trace ("RipCon("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 1
  applyRippleC (head cs:a) b carry
  freeConst [head cs]
  return (a, b )
    where applyRippleC (a:[]) [] z = cnot a z 
          applyRippleC (a0:a1:as) (b0:bs) z
            = do majC a0 b0 a1 
                 applyRippleC (a1:as) bs z
                 umaC a0 b0 a1
          majC x y z  
            = do tof ctrl z y
                 cnot z x
                 tof x y z
          umaC x y z  
            = do tof x y z
                 cnot z x
                 tof ctrl x y

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
    where start x (y:[]) (c:cs) = tof x y c
          start x (y:ys) (c:cs) = do tof x y c
                                     start x ys cs 
          applyAdders [] b out = return ()
          applyAdders (x:xs) ys out = do simpleCtrlRipple x ys (take (length ys) out) $ out !! length ys
                                         applyAdders xs ys (tail out)

--Below are things for karatsuba.  Should be 
mkKaratsuba :: [String] -> [String] -> Circuit
mkKaratsuba aLns bLns = circ
  where (_,(_,_,circ)) = runState go ([],map (\x->'c':show x) [100..999],Circuit (LineInfo [] [] [] []) [] [])
        go             = do a <- initLines aLns
                            b <- initLines bLns
                            mOut <- karatsuba a b  
                            setOutputs $ a ++ b ++ mOut

sTrace a = trace (show a) a 

karatsuba :: [String] -> [String] -> CircuitState [String]
karatsuba a b = assert (trace ("kara("++(show.length) a++","++ (show.length) b ++")") $ length a == length b) go
  where go | length a <= cuttoff = simpleMult a b 
           | otherwise           = karaR  
          where cuttoff = 5 --11 was found to be the best value (may be incorrect do to reduction of one adder
                karaR = do a0b0 <- karatsuba a0 b0  
                           a1b1 <- karatsuba a1 b1
                           padding <- getConst $ 2* (s0 - s1)
                           zs <- getConst 2
                           (_, a0plusa1) <- simpleRipple a0 (a1 ++take (s0-s1) padding) (head zs)
                           (_, b0plusb1) <- simpleRipple b0 (b1 ++ drop (s0-s1) padding) (zs!!1)
                           a0a1b0b1 <- karatsuba a0plusa1 b0plusb1
                           padding' <- getConst $ (length a0a1b0b1 - length a0b0) + (length a0a1b0b1 - length a1b1)
                           simpleSubtract (a0b0 ++ take (length a0a1b0b1 - length a0b0) padding') a0a1b0b1
                           simpleSubtract (a1b1 ++ drop (length a0a1b0b1 - length a0b0) padding') a0a1b0b1
                           freeConst padding'
                           resultPad <- getConst $ length a
                           let bitsa1b1 = length a0a1b0b1 - length (drop s0 a0b0)
                           simpleRipple a0a1b0b1  (drop s0 a0b0 ++ take bitsa1b1 a1b1) (a1b1!!bitsa1b1)
                           simpleRipple a0 (a1++take (s0-s1) padding) (head zs)
                           simpleRipple b0 (b1++drop (s0-s1) padding) (zs!!1)
                           freeConst zs
                           freeConst padding
                           return $ a0b0 ++ a1b1
                a0 = take s0 a
                a1 = drop s0 a
                b0 = take s0 b
                b1 = drop s0 b
                (s0,s1) = (\(_,x) -> orderT x) $ karaM cuttoff $ length a 

karaM :: Int -> Int -> (Int,(Int,Int))
karaM = Memo.memo2 Memo.integral Memo.integral kara 

kara :: Int -> Int -> (Int,(Int,Int))
kara c n | n <= c = (4*n^2 - 2*n - 1,(div n 2, n - div n 2))  
         | otherwise = minimumBy (compare `on` fst) $ karaSizes c $ splits n

ripAdd :: Int -> Int
ripAdd n = 2*n

naiveMult :: Int -> Int
naiveMult n = 4*(n^2) - 2*n -1  

karaSizes :: Int -> [(Int,Int)] -> [(Int,(Int,Int))]
karaSizes c =  map (\x -> (kSize x, x))
  where kSize x = karas x + adders x
        karas x = 2 * fst (karaM c (maxT x))  + fst(karaM c (minT x))
        adders x = 4*ripAdd (maxT x * 2 - mod (maxT x) 2) + 4 * ripAdd (maxT x)

orderT :: (Int,Int) -> (Int,Int)
orderT x = (maxT x, minT x)

maxT :: (Int,Int) -> Int 
maxT (x1,x2) = max x1 x2

minT :: (Int,Int) -> Int 
minT (x1,x2) = min x1 x2

splits :: Int -> [(Int,Int)]
splits  n = zip l $ reverse l
  where l = [s..e]
        s = div n 4 
        e = 3*s + mod n 4
