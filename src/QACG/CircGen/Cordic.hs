module QACG.CircGen.Cordic
(
cordic
)
where 

import QACG.CircUtils.CircuitState

import QACG.CircGen.Add.SimpleRipple(simpleSubtract)
import QACG.CircGen.Mult.SimpleMult(simpleMult)
import QACG.CircGen.Bit.MCToff(mctToff)
import CircGen.Misc(setNum)


-- For lookup table
angles = [atan (2**(-x)) | x <- [0..27]]

-- K(n) = \prod_{i=0}^{n-1} 1/\sqrt{1 + 2^{-2i}} 
K n = product [1 / sqrt (1+2**(-2*i))| i <- [0..n]]


toBin :: Int -> [Bool]
toBin 0 = []
toBin n = (mod n 2 == 1) : (toBin $ div n 2)

lookup :: [String] -> [String] -> [Int] -> CircuitState ()
lookup inputs outputs table = applyTable 0 binTable inputs outputs 
  where binTable = map toBin table
        applyTable n (t:ts) = do applyToff n t
                                 applyTable (n+1) ts 
        applyToff n targs = do setNum n inputs
                               mctToff inputs outputs
                               setNum n inputs 
          where out = snd $ unzip $ filter (\x-> fst x) $ zip targs outputs

-- Computes sin and cos for an angle 'beta' where -pi/2 < beta < pi/2
cordic :: Int -> [String] -> CircuitState ([String],[String])
cordic n beta = sigma <- getSigma
  where size = length beta
        iteration n v1 v2 b ang = do sigma <- getSigma 
                                     pad1 <- getConst n-1
                                     pad2 <- getConst n-1
                                     v1'  <- copy v1
                                     v2'  <- copy v2
                                     v1'' <- return drop (n-1) v1'++pad1
                                     v2'' <- return drop (n-1) v2'++pad2
                                     invert v2''
                                     cInvert sigma v1''
                                     cInvert sigma v2''
                                     (_,v1''')  <- simpleRipple v1 v2''
                                     (_,v2''')  <- simpleRipple v2 v1''
                                     cInvert sigma a
                                     invert a
                                     (_,b') <- simpleRipple b a 
                                     --a' = lookup (n
                                 
        getSigma b = do cs <- getConst size
                        sigma <- lessThen b cs
                        freeConst cs
                        return sigma
                           

lessThen :: [String] -> [String] -> CircuitState (String)
lessThen a b = assert (trace ("rip("++(show.length) a++","++(show.length) b++")") $ length a == length b) $ do
  cs <- getConst 1
  res <- getConst 1
  applyComp (head cs:a) b (head res)
  freeConst cs
  return (head res)
    where applyComp (a:[]) [] z = cnot a z 
          applyComp (a0:a1:as) (b0:bs) z
            = do rightTof x y z
                 cnot z x
                 cnot x y
                 applyComp (a1:as) bs z
                 cnot x y
                 cnot z x
                 rightTof x y z

invert :: [String] -> CircuitState ()
invert (x:xs) = do notgate x
                   invert xs
invert [] = return ()


cInvert :: String -> [String] -> CircuitState ()
cInvert c (x:xs) = do cnot c x
                      cInvert c xs
cInvert _ [] = return ()

copy :: [String] -> CircuitState ([String])
copy x = do res <- getConst $ length x
                   applyCopy x res
                   return res
  where applyCopy (a:as) (b:bs) = do cnot a b 
                                     applyCopy as bs
        applyCopy _ _ = return ()

