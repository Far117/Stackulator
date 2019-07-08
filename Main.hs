module Main where

import System.IO (hFlush, stdout)
import Data.List (isInfixOf)

import Calculator

-- | Given a Function and a list of Functions, checks if the single Function is already defined
-- | in the Function list by name. If it is, it replaces it. If it isn't, it is appended to the list.
updateFuncs :: [(String, Function)] -> Function -> [(String, Function)]
updateFuncs [] f@(Function n _ _)                    = [(n, f)]
updateFuncs (f@(name, _) : fs) func@(Function n _ _) = if (name == n) then (n, func):fs else f : updateFuncs fs func

-- | Handles the REPL
ui :: [(String, Function)] -> IO ()
ui fs = do
  putStr "> "
  hFlush stdout
  input <- getLine
  if (isInfixOf ":=" input) then ui (updateFuncs fs (toFunction input)) else do
    putStrLn . show $ evaluate (parseInput input) fs
    ui fs

main :: IO ()
main = putStrLn "Welcome to Stackulator" >> ui []
