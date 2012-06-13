module Main where

import System.Console.GetOpt
import System.Environment
import Control.Monad

import Scanner
import Parser
import AST
import Typing
import IR
import Intel

data Flag = Help | TypeCheck | PrettyPrint deriving (Eq, Show)

parseSuccess :: Maybe (a, [Token]) -> a
parseSuccess (Just (x,[])) = x
parseSuccess (Just (_,xs)) = error $ "Parse error starting at block'" ++ concat (map show xs) ++ "'"
parseSuccess Nothing = error "Parse error: could not read anything."

scanSuccess :: Maybe ([Token], String) -> [Token]
scanSuccess (Just (x,"")) = x
scanSuccess (Just (_,xs)) = error $ "Scan error at block '" ++ (take 20 xs) ++ "'"
scanSuccess Nothing = error "Scan error: could not read anything."

options :: [OptDescr Flag]
options =
     [ Option ['t']     ["typecheck"] (NoArg TypeCheck)           "shows type checking"
     , Option ['p']     ["prettyprint"] (NoArg PrettyPrint)       "pretty print input"
	 , Option ['h','?'] ["help"] (NoArg Help)       			  "this help menu"
     ]

-- Compile a .spl file
main = do
	args <- getArgs
	case getOpt RequireOrder options args of
		( _, [], _ ) -> putStrLn (usageInfo header options)
		([], [s], _) -> do 
						f <- readFile s
						return (progTypeCheck (prog f))
						mapM_ putStrLn (assemble $ convertProg (prog f))
		(opts, [s], [])
			| Help `elem` opts -> putStrLn (usageInfo header options)
		 	| PrettyPrint `elem` opts -> do
				f <- readFile s
				mapM_ putStrLn (prettyPrint (prog f))
		 	| TypeCheck `elem` opts -> do 
						f <- readFile s
						putStrLn $ showEnv (progTypeCheck (prog f))
		(_,_,errs)    -> error (concat errs)
	where
		prog s = (parseSuccess . progParse. scanSuccess. lineScan) s
		header = "Usage: splc [-tph] [.spl file]"


