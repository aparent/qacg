module CircUtils.QacgBool
( BExp(BVar,BConst,BNode)
, BOp  
, xor
, CircUtils.QacgBool.and  
, simplify 
, evaluate
, flatten
, vars
) where  

import qualified Data.List as List
import Data.Maybe
import Control.Applicative

data BExp =  BVar String 
           | BConst Bool   
           | BNode  BOp [BExp] deriving (Eq, Ord)
data BOp = XOR | AND deriving (Eq, Ord)

instance Show BExp where
  show (BVar a) =  a  
  show (BConst True) = show 1 
  show (BConst false) = show 0
  show (BNode AND a) = List.concat (List.intersperse "&" (map show a))
  show (BNode XOR a) = "(" ++ List.concat (List.intersperse "+" (map show a)) ++ ")"

instance Show BOp where
  show XOR = "+"
  show AND = "&"

xor :: [BExp] -> BExp
xor a = BNode XOR a

and :: [BExp] -> BExp
and a = BNode AND a

simplify :: BExp -> BExp
simplify = reduceXor.reduceAnd.combine

combine :: BExp -> BExp
combine (BVar a) = BVar a
combine (BConst a) = BConst a
combine (BNode op a) = BNode op (concatMap comb $ map combine a)
  where comb (BVar a) = [BVar a]
        comb (BConst a) = [BConst a]
	comb (BNode _ (y:[])) = [y]
	comb (BNode op2 y) = if op == op2 then y else [BNode op2 y]

reduceAnd :: BExp -> BExp
reduceAnd (BVar a) = BVar a
reduceAnd (BConst a) = BConst a
reduceAnd (BNode XOR a) = BNode XOR (map reduceAnd a)
reduceAnd (BNode AND a) = BNode AND (List.sort $ List.nub $ map reduceAnd a)

reduceXor :: BExp -> BExp
reduceXor (BVar a) = BVar a
reduceXor (BConst a) = BConst a
reduceXor (BNode AND a) = BNode AND (map reduceXor a)
reduceXor (BNode XOR a) = BNode XOR (reduce $ map reduceXor a)
  where reduce terms = onlyOdd $ (List.group.List.sort)  terms
        onlyOdd :: [[a]] -> [a] 
        onlyOdd [] = []
        onlyOdd (x:xs) = if ((length x) `mod` 2) == 1 
                       then (head x):(onlyOdd xs) 
                       else onlyOdd xs


flatten :: BExp -> BExp
flatten a 
  | exp == exp' = exp
  | otherwise = flatten exp'
  where exp  = a
        exp' = simplify $ distAnd exp 

distAnd :: BExp -> BExp
distAnd (BNode AND a) = BNode XOR ( foldr1 (\x y -> map CircUtils.QacgBool.and (pairs x y)) (map (val.distAnd) a) )
  where pairs x y = (\xx yy -> [xx,yy]) <$> x <*> y
        val (BNode _ a) = a
        val a = [a]
distAnd (BNode XOR a) = BNode XOR (map distAnd a)
distAnd a = a


vars :: BExp -> [String]
vars a = List.sort $ List.nub $ findVars a
  where findVars (BVar a) = [a]  
        findVars (BNode _ a) = concatMap findVars a  
        findVars _ = []

evaluate :: [Bool] -> BExp -> Bool
evaluate _ (BNode _ []) = False
evaluate vals exp = eval $ (replaceVars vals) exp
  where replaceVars (val:vals) exp 
          | length (vars exp) == 0 = exp 
          | otherwise = replaceVars vals exp'
	  where exp' = replaceVar (curVar,val) exp
	        curVar = head $ vars exp
        eval (BNode AND exps) = foldr1 (&&) $ map eval exps
        eval (BNode XOR exps) = foldr1 (\x y -> (x||y) && not (x&&y)) $ map eval exps
        eval (BConst v) = v


replaceVar :: (String,Bool) -> BExp -> BExp
replaceVar val (BNode op exps) = BNode op (map replaceVar' exps)
  where replaceVar' = replaceVar val
replaceVar (name,val) (BVar a) = if a == name 
                                 then BConst val 
                                 else BVar a
replaceVar _ a = a 
