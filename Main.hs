module Main where

import System.Environment
import Control.Monad

import Scanner
import Parser
import AST

parseSuccess :: Maybe (a, [Token]) -> a
parseSuccess (Just (x,[])) = x
parseSuccess (Just (_,xs)) = error $ "Parse error starting at block'" ++ show (take 4 xs) ++ "'"
parseSuccess Nothing = error "Parse error: could not read anything."

scanSuccess :: Maybe ([Token], String) -> [Token]
scanSuccess (Just (x,"")) = x
scanSuccess (Just (_,xs)) = error $ "Scan error at block '" ++ (take 20 xs) ++ "'"
scanSuccess Nothing = error "Scan error: could not read anything."

filterNL = filter (flip notElem "\r\n\t")

-- read a file
main = do
   [s] <- getArgs
   f <- readFile s
   let prog =  (parseSuccess . progParse. scanSuccess. lineScan . filterNL) f
   mapM_ putStrLn $ prettyPrint prog


