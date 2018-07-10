module Spring13.Week6.Fibonacci
  (
  fib
  ) where

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show stream = show (take 20 [value | value <- streamToList stream])

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

streamToList :: Stream a -> [a]
streamToList (Cons num nextStream) = num : streamToList nextStream

streamFromList :: [a] -> Stream a
streamFromList (x:xs) = Cons x (streamFromList xs)

getStreamOfNumbers :: (Enum a, Num a) => Stream a
getStreamOfNumbers = streamFromList [0..]

streamRepeat :: a -> Stream a
streamRepeat val = Cons val (streamRepeat val)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap func (Cons val nextStream) = Cons (func val) (streamMap func nextStream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed func input = Cons (input) (streamFromSeed func (func input))

nats :: Stream Integer
nats = streamFromList [0..]

ruler :: Stream Integer
