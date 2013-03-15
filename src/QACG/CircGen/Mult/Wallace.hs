module CircGen.Mult.Wallace
( wallaceMult
) where

import CircUtils.Circuit
import CircGen.Mult.PartialProduct
import CircGen.Add.SimpleRipple

import Data.List
import Data.Function (on) 

wallaceMult :: Int -> Circuit
wallaceMult a =  Circuit (clines a) (cgates a) (sub a)

sub :: Int -> [(Circuit,String)]
sub a = [adder,pproduct,fa,ha]
  where adder = (simpleRipple (adderSize a), "ADD" ++ show (adderSize a))
        pproduct = (partialProduct a , "PP" ++ show a)
        fa = (fullAdder, "AD")
        ha = (halfAdder, "HA")

adderSize :: Int -> Int
adderSize a = length $ dropWhile (\x-> length x > 2) (snd $ genAdders a)

clines :: Int -> LineInfo
clines a =  LineInfo { vars = circLines a
            , inputs = inputLines a   
            , outputs = outputLines a
            , outputLabels = outputLines a }

circLines :: Int -> [String] 
circLines a = 
  let adders = fst $ wallace [] $ wireList a
      lines  = sortedPerm a
  in  inputLines a ++ formatWires adders lines ++ adderAncilla a

inputLines ::  Int -> [String]
inputLines a = [ l++n |  l <- ["a","b"], n <- map show [0..(a-1)]]

outputLines :: Int -> [String]
outputLines a = map (convWire.head) flines ++ ["z"]
  where (_ , flines) = genAdders a


adderAncilla :: Int -> [String]
adderAncilla a = "ac" : go 0 adderLines ++ ["z"]
  where adderLines = dropWhile (\x-> length x > 2) (snd $ genAdders a)
	go _ []     = []
        go c (w:ws) = 
          if length w < 2 
          then ("ac" ++ show c) : go (c+1) ws 
          else go c ws

formatWires :: [Adder] -> [Wire] -> [String]
formatWires adders wires =
  let convertedWires =  map (\a -> (NORM,a)) wires
   in applyQCFormat 0 $ addAncilla adders convertedWires

data WireType = ANCILLA|NORM deriving(Show)

addAncilla :: [Adder] -> [(WireType,Wire)] -> [(WireType,Wire)]
addAncilla [] w = w
addAncilla ((t,_,h,f):as) w = case t of 
  HALF -> addAncilla as (insertAncilla hAn w)
  FULL -> addAncilla as (insertAncilla fAn w)
  where fAn = (ANCILLA,f)
        hAn = (ANCILLA,h)
        insertAncilla = insertBy (compare `on` (\(_,(a,b)) -> a+b))

applyQCFormat :: Int -> [(WireType,Wire)] -> [String]
applyQCFormat _ [] = []
applyQCFormat c ((NORM,w):ws) = convWire w : applyQCFormat c ws 
applyQCFormat c ((ANCILLA,_):ws) = ('c':show c ) : applyQCFormat (c+1) ws

cgates :: Int -> [Gate]
cgates a =  (partialProductCirc a : wallaceGates a) ++ [finalAdder a]

fullAdder = Circuit (LineInfo ["a","b","c","0"] [] [] [])
  [Gate "tof" ["c","0"]
  , Gate "tof" ["0","c"]
  , Gate "tof" ["a","b","c"]
  , Gate "tof" ["a","b"]
  , Gate "tof" ["b","0","c"]
  , Gate "tof" ["0","b"]] []
halfAdder = Circuit (LineInfo ["x","y","c"] [] [] [])
  [Gate "tof" ["y","c"]
  , Gate "tof" ["c","y"]
  , Gate "tof" ["x","c","y"]
  , Gate "tof" ["c","x"]] []

wallaceGates :: Int -> [Gate]
wallaceGates a = let adders = fst $ genAdders a
 		in iterGates 0 adders

iterGates :: Int -> [Adder] -> [Gate]
iterGates _ [] = [] 
iterGates c ((t,x,y,z):xs) = case t of
  FULL-> Gate "AD" [xstr,ystr,zstr,cstr] : iterGates (c+1) xs
  HALF-> Gate "HA" [xstr,ystr,cstr] : iterGates (c+1) xs
  where xstr = convWire x
        ystr = convWire y
        zstr = convWire z
        cstr = 'c' : show c

convWire :: Wire -> String
convWire (a,b) = "a"++show a ++ "b" ++ show b 

partialProductCirc :: Int -> Gate
partialProductCirc a = Gate ("PP"++show a) (inputLines a ++ formatWires [] (sortedPerm a))

finalAdder :: Int -> Gate
finalAdder a = 
  let addName = "ADD" ++ show (adderSize a)
      addGates = "ac": adderInputs 0 (snd $ genAdders a) ++ ["z"]
  in Gate addName addGates

adderInputs :: Int -> [[Wire]] -> [String]
adderInputs _ [] = [] 
adderInputs c (x:xs) 
  | length x == 1 = ("ac"++show c):convWire (head x):adderInputs (c+1) xs
  | otherwise     =  convWire (last x) : convWire (head x) : adderInputs c xs

type Wire = (Int,Int)

wirePerm :: Int -> [Wire]
wirePerm a = [ (x,y) | x<-[0..(a-1)] , y<-[0..(a-1)] ]

sortedPerm :: Int -> [Wire]
sortedPerm a = sortBy (compare `on` uncurry (+)) (wirePerm a)

wireList :: Int -> [[Wire]]
wireList a = groupBy ((==) `on` uncurry (+)) (sortedPerm a)

data AdderType = FULL|HALF deriving (Show)  
type Adder = (AdderType,Wire,Wire,Wire)
genAdders :: Int -> ([Adder],[[Wire]])
genAdders a = wallace [] (wireList a)

wallace :: [Adder] -> [[Wire]] -> ([Adder],[[Wire]])
wallace a w 
  | maximum (map length w) > 2 = wallace (a++adders) wires
  | otherwise  = (a,w)
  where adders = fst $ iterWallace w
        wires  = snd $ iterWallace w

iterWallace :: [[Wire]] -> ([Adder],[[Wire]])
iterWallace wires = 
  let w = map applyWallace wires
      adders = foldl' (++) [] (fst $ unzip w)
      groupedWires = combWires [] (snd $ unzip w) 
  in  (adders,groupedWires) 

combWires :: [Wire] -> [([Wire],[Wire])] ->  [[Wire]]
combWires [] []       = []
combWires  z []       = [z]
combWires  z ((x,y):xs) = (x++z):combWires y xs 

applyWallace :: [Wire] -> ([Adder],([Wire],[Wire]))
applyWallace wires 
  | null wires         = ([],([],[]))
  | length wires == 1  = ([],(wires,[]))
  | length wires == 2  = applyHalfAdder wires
  | length wires  > 2  = combine (applyFullAdder wires) (applyWallace $ drop 3 wires)
  where combine (a,(b,c)) (d,(e,f)) = (a++d,(b++e,c++f))
        applyHalfAdder (x:y:[]) = ([(HALF,x,y,(0,0))],([x],[y]))
        applyFullAdder (x:y:z:_) = ([(FULL,x,y,z)],([y],[z]))

