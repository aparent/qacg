module Main( main ) where

import Data.Maybe
import System.Console.GetOpt
import System.Environment
import System.Exit

main = do  
  args <- getArgs
  case getOpt RequireOrder options args of
    (flags, [],      [])     -> print $ flags
    (_,     nonOpts, [])     -> error $ "unrecognized arguments: " ++ unwords nonOpts
    (_,     _,       msgs)   -> error $ concat msgs ++ usageInfo header options


data Flag 
  = Verbose  | Version 
  | Input String | Output String | LibDir String
  deriving Show

options :: [OptDescr Flag] 
options = [ 
    Option ['o'] ["output"] (ReqArg Output "FILE") "specify out file", 
    Option ['V'] ["version"] (NoArg Version) "shows the version number" 
  ]

header = "Usage: CircLib [OPTION...]"

--inp,outp :: Maybe String -> Flag
--outp = Output . fromMaybe "stdout"
--inp  = Input  . fromMaybe "stdin"
