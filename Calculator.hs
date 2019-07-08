{-# OPTIONS_GHC -Wall #-}

module Calculator where

import Data.List.Split (splitOn)
import Data.List (isInfixOf)

import Stack

-- | Name, args, body
data Function = Function String [String] (Stack String)
  deriving (Show)

-- | Given a String such as "f a b := a b +", isolates the function's name,
-- | parameters, and body, wrapping it in a Function datatype.
toFunction :: String -> Function 
toFunction str = let parts  = splitOn ":=" str
                     header = head parts
                     body   = fromList . words . head $ tail parts
                     name   = head $ words header
                     args   = drop 1 $ words header in
                   Function name args body

-- | Turns a list of Functions into a list of pairs of Functions and their names.
fNameList :: [Function] -> [(String, Function)]
fNameList fs = map (\f@(Function name _ _) -> (name, f)) fs

-- | Takes a Function and a list of arguments, and fills in the Function's
-- | parameters with the arguments. Returns a Stack containing the new instructions
-- | to be executed.
fillIn :: Function -> [String] -> Stack String
fillIn (Function _ params stack) args = loop (zip params args) stack where
  loop _ Bottom                 = Bottom
  loop argList (Stack val rest) = case (lookup val argList) of
                                    Nothing -> Stack val $ loop argList rest
                                    Just v  -> Stack v   $ loop argList rest

-- | Takes arguments from the current working Stack to apply fill in a Function with.
-- | Returns a Stack minus the arguments taken, and a Stack representing new instructions
-- | to evaluate.
expand :: (Real a, Fractional a, Show a) => Function -> Stack a -> (Stack a, Stack String)
expand f@(Function _ params _) stack = let (args, stack') = popN (length params) stack in
                                         (stack', fillIn f (map show args))

-- | Evaluates a Stack of instructions which does not contain user-defined Functions.
evalBase :: (Real a, Read a, Fractional a) => Stack String -> Stack a
evalBase stack = foldl step Bottom (toList stack) where
  step :: (Real a, Read a, Fractional a) => Stack a -> String -> Stack a
  step stk tok = case lookup tok symbols of
                   Nothing -> push (read tok) stk
                   Just f  -> apply2 f stk

-- | Default functions.
symbols :: (Real a, Fractional a) => [(String, a -> a -> a)]
symbols = [
            ("+", (+)),
            ("-", (-)),
            ("*", (*)),
            ("/", (/))
          ]

-- | Applies a binary function to the first two elements of a Stack,
-- | pushing the result onto the Stack.
apply2 :: (a -> a -> a) -> Stack a -> Stack a
apply2 f (Stack a (Stack b t)) = Stack (f a b) t
apply2 _ _                     = error "Error: Stack bottomed out on function application!"

-- | Turns a String into a Stack where each element is a word.
parseInput :: String -> Stack String
parseInput = fromList . words

-- | Evaluates a Stack of instructions. Each token may either be a number,
-- | a symbol, or a user-defined Function. Returns the final working Stack.
evaluate :: (Read a, Show a, Real a, Fractional a) => Stack String -> [(String, Function)] -> Stack a
evaluate input fs = loop input fs Bottom where
  loop Bottom _ result          = result
  loop (Stack tok r) fs' stack  = case lookup tok symbols of
                                    Just f  -> loop r fs' $ apply2 f stack
                                    Nothing -> case lookup tok fs' of
                                                 Just f  -> let (s', h') = expand f stack in
                                                              loop (pushStack r h') fs' s'
                                                 Nothing -> loop r fs' $ Stack (read tok) stack
