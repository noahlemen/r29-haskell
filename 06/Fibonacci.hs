module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate getNextFibTuple (0, 1)

getNextFibTuple :: (Integer, Integer) -> (Integer, Integer)
getNextFibTuple (x, y) = (y, x + y)
