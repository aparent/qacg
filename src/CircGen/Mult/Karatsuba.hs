module CircGen.Mult.Karatsuba
( karatsuba
  ,mkKaratsuba
) where

import qualified Data.MemoCombinators as Memo
import Control.Monad.State

import CircUtils.Circuit
import CircUtils.CircuitState

import CircGen.Add.SimpleRipple
import CircGen.Mult.SimpleMult


mkKaratsuba :: [String] -> [String] -> Circuit
mkKaratsuba aLns bLns = circ
  where (_,(_,_,circ)) = runState go ([],map (\x->'c':show x) [100..999],Circuit (LineInfo [] [] [] []) [] [])
        go             = do a <- initLines aLns
                            b <- initLines bLns
                            mOut <- karatsuba a b  
                            setOutputs $ a ++ b ++ mOut


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

sTrace a = trace (show a) a 

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
