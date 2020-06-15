module List.Stack where

import List.LinearList

-- Stack

type Stack = List

testStack = Cons 1 $ Cons 20 $ Cons 4 $ Cons 3 $ Cons 50 Nil :: Stack Integer

-- Methods

push :: a -> Stack a -> Stack a
push = Cons

pop :: Stack a -> Stack a
pop Nil = error "Stack is empty"
pop (Cons x s) = s

top :: Stack a -> a 
top Nil = error "Stack is empty"
top (Cons x s) = x

