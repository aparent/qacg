
import Control.Parallel.Strategies as P
import Data.List(minimumBy) 
import Data.Function(on)
import qualified Data.MemoCombinators as Memo
import System.IO

applyKara :: Int -> Int -> [String] -> [String] -> [String] -> ([Gates],[String],[String],[String])
applyKara cutoff n consts x y = (gates, outputs, constsLeft, constsUsed)
  where smallKara = minT $ karaM cutoff n
        bigKara   = maxT $ karaM cutoff n
				(gK0, outK0, constLK0, constUK0) = applyKara cutoff smallKara consts 

data KTree = Split (Int,Int) KTree KTree | Cut Int 

instance Show KTree where
	show = showT 0

showT n (Split (a,b) (Cut _) (Cut _)) = tabs n ++ show a ++ "-" ++ show b 
showT n (Split (a,b) (Cut _) r      ) = tabs n ++ show a ++ "-" ++ show b ++ "\n" ++ showT (n+1) r 
showT n (Split (a,b) l       (Cut _)) = tabs n ++ show a ++ "-" ++ show b ++ "\n" ++ showT (n+1) l
showT n (Split (a,b) l       r      ) = tabs n ++ show a ++ "-" ++ show b ++ "\n" ++ showT (n+1) l ++ "\n" ++ showT (n+1) r 

tabs n = take n $ repeat '\t'

mktree :: Int -> Int -> KTree
mktree c x | r > 0 = Split (l,r) (mktree c l) (mktree c r)
				   | otherwise = Cut l 
				   where (_,(l,r)) = karaM c x


--Below are functions that calculate the optimal split
spaceSep :: (Show a , Show b) => [(a,b)] -> String
spaceSep = foldl (\x (y1,y2) -> x ++ show y1 ++" " ++ show y2 ++ "\n") "" 

karaM :: Int -> Int -> (Int,(Int,Int))
karaM = Memo.memo2 Memo.integral Memo.integral kara 

avgCutoff :: (Int,Int) -> Int -> (Int,Float)
avgCutoff r c = (c,fAvg (fst.karaM c) r)

kara :: Int -> Int -> (Int,(Int,Int))
kara c n | n <= c = (4*n^2 - 2*n - 1,(n,0))  
         | otherwise = minimumBy (compare `on` (\(x,y)-> x)) $ karaSizes c $ splits n

ripAdd :: Int -> Int
ripAdd n = 2*n

naiveMult :: Int -> Int
naiveMult n = 4*(n^2) - 2*n -1  

karaSizes :: Int -> [(Int,Int)] -> [(Int,(Int,Int))]
karaSizes c n =  map (\x -> (kSize x, x)) n
  where kSize x = karas x + adders x
        karas x = 2 * fst (karaM c (maxT x))  + fst(karaM c (minT x))
        adders x = 4*(ripAdd $ (maxT x)*2-(mod (maxT x) 2)) + 4*(ripAdd $ maxT x)


maxT :: (Int,Int) -> Int 
maxT (x1,x2) | x1 > x2 = x1 
             | otherwise = x2

minT :: (Int,Int) -> Int 
minT (x1,x2) | x1 < x2 = x1 
             | otherwise = x2

splits :: Int -> [(Int,Int)]
splits  n = zip l $ reverse l
  where l = [s..e]
        s = div n 4 
        e = 3*s + mod n 4

fAvg :: (Int -> Int) -> (Int,Int) -> Float
fAvg f (x1,x2) = (fromIntegral $ sum $ map f [x1 .. x2]) / fromIntegral numTerms 
	where numTerms = x2 - x1 + 1

