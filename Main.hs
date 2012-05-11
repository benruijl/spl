module Main where

import System.Environment
import Control.Monad

import Scanner
import Parser
import AST
import Typing
import IR
import SSM

parseSuccess :: Maybe (a, [Token]) -> a
parseSuccess (Just (x,[])) = x
parseSuccess (Just (_,xs)) = error $ "Parse error starting at block'" ++ concat (map show xs) ++ "'"
parseSuccess Nothing = error "Parse error: could not read anything."

scanSuccess :: Maybe ([Token], String) -> [Token]
scanSuccess (Just (x,"")) = x
scanSuccess (Just (_,xs)) = error $ "Scan error at block '" ++ (take 20 xs) ++ "'"
scanSuccess Nothing = error "Scan error: could not read anything."

-- read a file
main = do
   [s] <- getArgs
   f <- readFile s
   let prog =  (parseSuccess . progParse. scanSuccess. lineScan) f
   mapM_ putStrLn (prettyPrint prog)
   putStrLn $ "\n" ++ showEnv (progTypeCheck prog) ++ "\n\n" ++ (unlines . assemble $ convertProg prog)


