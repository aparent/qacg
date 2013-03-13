module CircUtils.QacgBool
( BExpr(V,C,Xor,And)
, simplify 
, replaceVar
, evaluate
, flatten
) where

import qualified Data.List as List
import Control.Applicative

data BExpr = V String 
           | C Bool
           | Xor [BExpr]
           | And [BExpr]
           deriving (Eq, Ord)

instance Show BExpr where
  show (V a) =  a  
  show (C True) = show 1 
  show (C False) = show 0
  show (And a) = intercalate "&" (map show a)
  show (Xor a) = "(" ++ intercalate "+" (map show a) ++ ")"

simplify :: BExpr -> BExpr
simplify = reduceConst . combine . reduce

reduce :: BExpr -> BExpr
reduce (Xor a) = Xor (reduceXor $ map reduce a)
  where reduceXor = onlyOdd . List.group . List.sort
        onlyOdd [] = []
        onlyOdd (x:xs) = if (length x `mod` 2) == 1 
                         then head x : onlyOdd xs
                         else onlyOdd xs
reduce (And a) = And (reduceAnd $ map reduce a)
  where reduceAnd = List.sort . List.nub
reduce a = a

reduceConst :: BExpr -> BExpr
reduceConst (And a) | any ( \x -> x == C False ) a'  = C False 
                    | otherwise =  And (removeOnes a')
  where a' = map reduceConst a
        removeOnes [] = []
        removeOnes (C True:xs)  = removeOnes xs 
        removeOnes (x:xs) = x : removeOnes xs
reduceConst (Xor a)  | length a' >= 1 = Xor (removeZeros a') 
                     | otherwise      = C False
  where a' = map reduceConst a
        removeZeros [] = []
        removeZeros (C False:xs)  = removeZeros xs 
        removeZeros (x:xs) = x : removeZeros xs
reduceConst a = a        

combine :: BExpr -> BExpr
combine (And a) = And (concatMap comb $ map combine a)
  where comb (Xor x) = [Xor x]
	comb (And x) = x
        comb a = [a]
combine (Xor a) = Xor (concatMap comb $ map combine a)
  where comb (And x) = [And x]
	comb (Xor x) = x
        comb a = [a]
combine a = a

flatten :: BExpr -> BExpr
flatten a 
  | exp == exp' = exp
  | otherwise = flatten exp'
  where exp  = a
        exp' = simplify $ distAnd exp 


-- | This function distributes ands into xor functions eg. a(b+c) = ab + ac.
--   Note: This function assumes that the combine function has already been run on the expression
distAnd :: BExpr -> BExpr
distAnd (And a) = if length andPairs > 1 then Xor andPairs else head andPairs
  where andPairs = foldr1 (\x y -> map And $ pairs x y) (map (val . distAnd) a ) 
        pairs x y = (\xx yy -> [xx,yy]) <$> x <*> y --Returns all x/y pairs ie (a + b)(c + d) = ac + ad + bc + bd
        val (Xor a) = a
        val a = [a]
distAnd (Xor a) = Xor $ map distAnd a
distAnd a = a

replaceVar :: (String,Bool) -> BExpr -> BExpr
replaceVar (name,val) (V a) = if a == name  
                              then C val  
                              else V a
replaceVar val (Xor a) = Xor $ map (replaceVar val)  a
replaceVar val (And a) = And $ map (replaceVar val)  a
replaceVar _ (C a) = C a

vars :: BExpr -> [String]
vars a = List.sort $ List.nub $ findVars a
  where findVars (V a) = [a]  
        findVars (C _) = []
        findVars (Xor a) = concatMap findVars a  
        findVars (And a) = concatMap findVars a  

evaluate :: [Bool] -> BExpr -> Bool
evaluate _ (Xor []) = False
evaluate _ (And []) = False
evaluate vals exp = eval $ replaceVars vals exp
  where replaceVars (val:vals) exp 
          | null (vars exp) = exp 
          | otherwise = replaceVars vals exp'
	  where exp' = replaceVar (curVar,val) exp
	        curVar = head $ vars exp
        eval (And exps) = all eval exps
        eval (Xor exps) = foldr1 (\x y -> (x||y) && not (x&&y)) $ map eval exps
        eval (C v) = v
