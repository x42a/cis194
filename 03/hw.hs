data IntList = Empty | Cons Int IntList
  deriving (Show)

absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty = Empty
squareAll (Cons x xs) = Cons (x * x) (squareAll xs)

mapIntList :: (Int -> Int) -> IntList -> IntList
mapIntList _ Empty = Empty
mapIntList f (Cons x xs) = Cons (f x) (mapIntList f xs)

exampleList = Cons (-1) (Cons 2 (Cons (-6) Empty))

addOne x = x + 1

square x = x * x

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
  | even x = Cons x (keepOnlyEven xs)
  | otherwise = keepOnlyEven xs

filterIntList :: (Int -> Bool) -> IntList -> IntList
filterIntList _ Empty = Empty
filterIntList f (Cons x xs)
  | f x = Cons x (filterIntList f xs)
  | otherwise = filterIntList f xs

-- Possible fold implementation. NOT WORKING
foldIntList :: (Int -> Int -> Int) -> IntList -> IntList
foldIntList _ Empty = Empty
foldIntList f (Cons x (Cons y ys)) = Cons (f x y) (foldIntList f ys)

-- POLYMORPHISM
data List t = E | C t (List t)

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x xs)
  | p x = C x (filterList p xs)
  | otherwise = filterList p xs

mapList :: (a -> b) -> List a -> List b
mapList _ E = E
mapList f (C x xs) = C (f x) (mapList f xs)

doStuff1 :: [Int] -> Int -- Partial: fn head
doStuff1 [] = 0
doStuff1 [_] = 0
doStuff1 xs = head xs + head (tail xs)

doStuff2 :: [Int] -> Int -- Better way: total fn
doStuff2 [] = 0
doStuff2 [_] = 0
doStuff2 (x1 : x2 : _) = x1 + x2