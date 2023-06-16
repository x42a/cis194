{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

import Text.XHtml (base)

-- Homework 06: https://www.seas.upenn.edu/~cis1940/spring13/hw/06-laziness.pdf

{-
  Fibonacci Numbers: a sequence of integers, beginning by 0 and 1, where every
  integer Fn in the sequence is the sum of the previous two:

  0,1,1,2,3,5,8,13,21,34,55,89,144,233,377

  That follows: F0 = 0; F1 = 1; ...; Fn = F(n-1) + F(n-2), n>=2
-}

-- EXERCISE 1: create a recursive function, fib, with the above definition, so
-- that fib n computes the nth Fibonacci number.
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Then define the infinite list of all the Fibonacci numbers:
fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- As can be seen by running on GHCI, this is extremly inneficient because of
-- the repeated evaluations of fib.

-- EXERCISE 2: create a more efficent implementation which requires only O(n)
-- addition operations, that is maximun n additions to get the first n elements.

-- This implementation makes use of a touple to store the last two values of the
-- series. The next value is the sum of those to values and the previously next
-- value is passed on the second element of the touple. Then the iteration is
-- mapped to the first element, which contain every value in the series.
-- Finally, 0 is added to the start of the list as it is the first element of
-- the original Fibonacci sequence.
fibs2 :: [Integer]
fibs2 = (0 :) . map fst $ iterate (\(n, m) -> (n + m, n)) (1, 0)

-- EXERCISE 3
-- Define a type of polymorphic streams (aka infinite lists with just one cons).
data Stream a = Stream a (Stream a)

-- Convert a Stream to an infinite list.
streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

-- Create an instance of Show Stream for testing purposes.
instance (Show a) => Show (Stream a) where
  show :: (Show a) => Stream a -> String
  show = show . take 20 . streamToList

-- EXERCISE 4
-- Write a function which generates a Stream containing infinitely many copies
-- of a given element.
streamRepeat :: a -> Stream a
streamRepeat x = Stream x $ streamRepeat x

-- Write a function which applies a function to every element of a Stream.
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) $ streamMap f xs

-- Write a function which generates a Stream from a seed, the first element of
-- the Stream, and applies a function to generate the next seed, infinitely.
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Stream a $ streamFromSeed f $ f a

-- EXERCISE 5
-- Define a Stream containing the ifinite list of natural numbers
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

-- Define a Stream which corresponds to the ruler function
-- Ruler function: the nth element in the stream is the largest power of 2
-- which evenly divides n, starting from n = 1.
ruler :: Stream Integer
ruler = streamMap (`mapper` 1) $ streamFromSeed (+ 1) 1
  where
    mapper n i
      | odd n = 0
      | even (n `div` (2 ^ i)) = mapper n (i + 1)
      | otherwise = i

-- Alternate the elements from two streams
interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream (Stream x xs) (Stream y ys) = Stream x $ interleaveStream ys xs

-- TODO: Ruler using interleaveStream, without making use of any divisibilty testing
ruler' :: Stream Integer
ruler' = streamMap (\n -> n `div` (n - 2)) $ streamFromSeed (+ 1) 3
  where
    mapper n i
      | even (n `div` (2 ^ i)) = mapper n (i + 1)
      | otherwise = i

-- Fibonacci Numbers via generating functions
-- a0 +a1x+a2x2 +···+anxn +... is a generative function
-- The coeficients will be stored in a Stream

-- EXERCISE 6

-- note that x = 0 + 1x + 0x2 + 0x3 + . . . .
x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

instance Num (Stream Integer) where
  (+) (Stream x xs) (Stream y ys) = Stream (x + y) (xs + ys)

  -- Supposing A = a0 + as and B = b0 + bs, AB = a0b0 + a0B' + A'B (more details
  -- in the homework sheet)
  (*) (Stream a0 as) s2@(Stream b0 bs) = Stream (a0 * b0) (streamMap (* a0) bs + (as * s2))
  fromInteger n = Stream n $ streamRepeat 0
  negate = streamMap negate

instance Fractional (Stream Integer) where
  (/) (Stream a0 as) (Stream b0 bs) = q
    where
      q = Stream (a0 `div` b0) (streamMap (`div` b0) (as - q * bs))

-- TODO: understand above definitions and implement the following exercises (6, 7)
fibs3 :: Stream Integer
fibs3 = streamFromSeed (\x -> x `div` (1 - x - x ^ 2)) 1