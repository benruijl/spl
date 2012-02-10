module Main where

import System.Environment
import Control.Monad

import Scanner
import Parser
import AST

identScan = (alphaScan >-> (\x -> [x])) # (iter alphaNumUnderScoreScan) >-> cat1

tokList = ["+", "-", "*", "/", "%", "==", "<", "<", ">", "<=", ">=", "!=", "&&", "||", ":", "!", "=", "(", ")", ";", "}", "{"]

tokScan = ((twoChar >-> cat2) ? inList) ! ((char >-> (\x -> [x])) ? inList) 
  where
  inList x = elem x tokList

-- note: intScan returns an empty string on total failure instead of nothing
intScan = (((matchChar '-') >-> (\x->[x])) # (iter digitScan)) >-> cat1 ! (iter digitScan)

-- line scan
lineScan = trim $ iter((token identScan) ! (token tokScan) ! (token intScan) ? (/=""))

scanSuccess :: Maybe (a, String) -> Maybe (a, String)
scanSuccess (Just(x,"")) = Just(x,"")
scanSuccess (Just(_,xs)) = error $ "Scan error at '" ++ xs ++ "'"
scanSuccess Nothing = fail "Scan error: could not read anything."

-- read a file
main = do
   [s] <- getArgs
   f <- readFile s
   let res =  map (scanSuccess . lineScan) (lines f)
   putStrLn $ show res


