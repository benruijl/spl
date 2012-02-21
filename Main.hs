module Main where

import System.Environment
import Control.Monad

import Scanner
import Parser
import AST

scanSuccess :: Maybe (a, String) -> a
scanSuccess (Just (x,"")) = x
scanSuccess (Just (_,xs)) = error $ "Scan error at '" ++ xs ++ "'"
scanSuccess Nothing = error "Scan error: could not read anything."

filterNL = filter (flip notElem "\r\n\t")

-- read a file
main = do
   [s] <- getArgs
   f <- readFile s
   let prog =  (scanSuccess . progParse. filterNL) f
   mapM_ putStrLn $ prettyPrint prog


