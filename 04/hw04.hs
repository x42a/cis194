fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (+ (-2)) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =
  sum
    . filter even
    . takeWhile (/= 1)
    . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- NOT WORKING
foldTree :: [a] -> Tree a
foldTree = foldr balanceTree Leaf
  where
    balanceTree x Leaf =
      Node 0 Leaf x Leaf
    balanceTree x (Node h Leaf y Leaf) =
      Node (h + 1) (balanceTree x Leaf) y Leaf
    balanceTree x (Node h treeL@(Node {}) y Leaf) =
      Node h treeL y (balanceTree x Leaf)
    balanceTree x t@(Node h treeL@(Node hL _ _ _) y treeR@(Node hR _ _ _))
      | hL > hR = Node h treeL y (balanceTree x treeR)
      | otherwise = middleBalanceTree t (balanceTree x treeL)
    middleBalanceTree (Node h treeL@(Node hL _ _ _) y treeR) treeLNext@(Node hLN _ _ _)
      | hL == hLN = Node h treeLNext y treeR
      | otherwise = Node (h + 1) treeLNext y treeR

xor :: [Bool] -> Bool
xor = odd . foldr (\b x -> if b then x + 1 else x) 0

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse

{-
  Algorithm to find all prime integers up to 2n + 2
  1. Remove all elements up to n in the form of i + j + 2ij
  2. Duplicate and add 1 to the remaining elements
 -}
sieveSundaram :: Integer -> [Integer]
sieveSundaram =
  map (((+ 1) . (* 2)) . fst)
    . filter (\(x, (_, rm)) -> x `notElem` rm)
    . takeWhile (\(x, (n, _)) -> x /= (n + 1))
    . zip [1 ..]
    . repeat
    . (\n -> (n, removed n)) -- optimized to call removed only once
  where
    removed n =
      map (\(i, j) -> i + j + 2 * i * j) $
        cartProd [1 .. (n + 1)] [1 .. (n `div` 2)]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]
