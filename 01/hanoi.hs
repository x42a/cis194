{- TOWERS OF HANOI:
The puzzle consists of three pegs and n-disks with different sizes.

The goal of the puzzle is to move all disks from one peg to
another, following these two rules:
  1. you can only move one disk at a time
  2. a larger disk may never be stacked on top of a smaller one

To achive this, we can use this recursive algorithm (move all disks
from a to b):
  1. move n-1 disks from a to c using b as temporary storage
  2. move the top disk from a to b
  3. move n-1 disks from c to b using a as temporary storage

Confusing? I know. But, let's think of a, b and c as
destinations, as in the solution bellow. -}

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 from to _ = [(from, to)]
hanoi disk from to spare =
  hanoi (disk - 1) from spare to
    ++ [(from, to)]
    ++ hanoi (disk - 1) spare to from

{- TOWERS OF HANOI: 4 pegs
Algorithm from https://dl.acm.org/doi/pdf/10.1145/126459.126460

Given k disks (k being described in the paper above and implemented bellow) at
the bottom of the "from" peg:
1. Move n-k disks from a to c using the algorithm
2. Move k disks from a to b (destination) using the 3-peg algorithm (hanoi)
3. Move n-k disks from c to b using the algorithm
-}
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 disks from to spare1 spare2 =
  hanoi4 (disks - k) from spare1 spare2 to
    ++ hanoi k from to spare2
    ++ hanoi4 (disks - k) spare1 to spare2 from
  where
    k = getK disks

getK :: Integer -> Integer
getK 1 = 1
getK 2 = 1
getK n = getKEquation n 1 1

getKEquation :: Integer -> Integer -> Integer -> Integer
getKEquation n i k
  | n == i = k
  | n < i = k - 1
  | otherwise = getKEquation n (i + nextK) nextK
  where
    nextK = k + 1

{- This doesn't work with disks > 5. TODO: fix when I have the math knowledge.
hanoi4 disk from to s1 s2 = hanoi4Idea disk from to s1 s2 True True
hanoi4Idea :: Integer -> Peg -> Peg -> Peg -> Peg -> Bool -> Bool -> [Move]
hanoi4Idea disk from to s1 s2 next b
  | disk == 1 = m2
  | next = m1 ++ m2 ++ m3
  | b = m1 ++ m2
  | not b = m2 ++ m3
  where
    m1 = hanoi4Idea (disk - 1) from s1 s2 to (not next) True
    m2 = [(from, to)]
    m3 = hanoi4Idea (disk - 1) s1 to s2 from (not next) False -}

hanoi42 :: Integer -> [Move]
hanoi42 disk = hanoi4 disk "a" "b" "c" "d"

hanoi2 :: Integer -> [Move]
hanoi2 disk = hanoi disk "a" "b" "c"