{- TOWERS OF HANOI:
The puzzle consists of three pegs and n-disks with different sizes.

The goal of the puzzle is to move all disks from one peg to
another, following these two rules:
  1. you can only move one disk at a time
  2. a larger disk may never be stacked on top of a smaller one

To achive this, we can use this recursive algorithm (to move all disks
from a to b):
  1. move n-1 disks from a to c using b as temporary storage
  2. move the top disk from a to b
  3. move n-1 disks from c to b using a as temporary storage -}

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi disk src dest aux
  | disk <= 0 = [] -- prevent usage with negative number of disks
  | disk == 1 = [(src, dest)]
  | otherwise =
      hanoi (disk - 1) src aux dest
        ++ [(src, dest)]
        ++ hanoi (disk - 1) aux dest src

{- TOWERS OF HANOI: 4 pegs
Algorithm from https://dl.acm.org/doi/pdf/10.1145/126459.126460

Given a total of n disks and a subset of k disks fixed at the bottom of the
source peg:
  1. Move n-k disks from a (src) to c (aux1) using this algorithm
  2. Move k disks from a (src) to b (dest) using the 3-peg algorithm (hanoi)
  3. Move n-k disks from c (aux1) to b (dest) using the algorithm

In all steps above, we use the d (aux2) peg, the new available peg, as temporary
storage

In reality k can be any random number between 1 and n-1, inclusive, but we can
get the value that results in the fewest moves by applying the equation on the
paper (getBestBottomSubset)
-}
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 disk src dest aux1 aux2
  | disk <= 0 = []
  | otherwise =
      hanoi4 (disk - k) src aux1 aux2 dest
        ++ hanoi k src dest aux2
        ++ hanoi4 (disk - k) aux1 dest aux2 src
  where
    k = findBestBottomSubset disk

findBestBottomSubset :: Integer -> Integer
findBestBottomSubset 1 = 1
findBestBottomSubset n = findBestBottomSubsetEquation n 1 1

findBestBottomSubsetEquation :: Integer -> Integer -> Integer -> Integer
findBestBottomSubsetEquation n i k
  | n == i = k
  | n < i = k - 1
  | otherwise = findBestBottomSubsetEquation n (i + nextK) nextK
  where
    nextK = k + 1

{- This doesn't work with disks > 5.
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

hanoi4Test :: Integer -> [Move]
hanoi4Test disk = hanoi4 disk "a" "b" "c" "d"

hanoiTest :: Integer -> [Move]
hanoiTest disk = hanoi disk "a" "b" "c"