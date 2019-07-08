{-# OPTIONS_GHC -Wall #-}

module Stack where

data Stack a = Bottom
             | Stack a (Stack a)
 deriving (Show)

-- | Returns a pair containing the head and tail of the Stack.
pop :: Stack a -> (a, Stack a)
pop Bottom      = error "Error: Cannot pop empty stack!"
pop (Stack a t) = (a, t)

-- | Returns the first n elements of the Stack, along with its tail.
popN :: Int -> Stack a -> ([a], Stack a)
popN 0 stack            = ([], stack)
popN _ Bottom           = error "Error: Cannot pop empty stack!"
popN n (Stack val rest) = let (f, s) = popN (pred n) rest in (val : f, s)

-- | Adds the given value to the head of the Stack.
push :: a -> Stack a -> Stack a
push a Bottom = Stack a Bottom
push a stack  = Stack a stack

-- | Adds a list of values to the head of the Stack.
pushAll :: [a] -> Stack a -> Stack a
pushAll [] stack     = stack
pushAll (x:xs) stack = push x $ pushAll xs stack

-- | Makes the first Stack the tail of the second Stack.
pushStack :: Stack a -> Stack a -> Stack a
pushStack s1 Bottom      = s1 
pushStack s1 (Stack v r) = Stack v $ pushStack s1 r

-- | Converts a Stack to a list
toList :: Stack a -> [a]
toList Bottom      = []
toList (Stack a t) = a : toList t

-- | Creates a Stack from a List
fromList :: [a] -> Stack a
fromList []    = Bottom
fromList (h:t) = Stack h (fromList t)
