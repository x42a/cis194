-- Week 5: More polymorphism and type classes

{-
  A polymorphyc type like:
  > a -> a -> a

  Ensures that a function with this type must always work, whatever the type a.
  This also refers to parametric polymorphism.

  The only functions that could have this type are:
  f1 x y = x
  f2 x y = y
-}

{-
  How does + work, if it can be called with different number types?! (Int,
  Integer, Double...). If we analyse its type:

  (+) :: Num a => a -> a -> a

  We see that something is going on at the start (Num a =>), which also happens
  in many other Prelude functions (==, /=, ...)

  Num is a type class, and + is respectively a type-class polymorphic function.

  For instance, let's take a look at the Eq class used in (==):

  class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

  As we can see, to make any type an instance of Eq we must defind those two
  functions.

  When a function takes an Eq a => a -> a -> Bool, like ==, what it is asking is
  any type class a which must be an instance of Eq
-}

-- Custom type instance of Eq with == comparison.
-- Note that is only necessary to define either == or /=, as the other is
-- automatically implied to be not (==) or not (/=), the opposite.
data Foo = F Int | G Char

instance Eq Foo where
  (==) :: Foo -> Foo -> Bool
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False

-- Eq can also automatically define the comparison itself:
data Foo' = F' Int | G' Char deriving (Eq, Ord, Show)

-- Defining custom type clases:
class Blerg a b where
  blerg :: a -> b -> Bool

class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  toList :: Int -> [Int]
  toList x = [x]

instance Listable Integer where
  toList :: Integer -> [Int]
  toList x = [fromIntegral x]

instance Listable Bool where
  toList :: Bool -> [Int]
  toList False = [0]
  toList True = [1]

instance Listable [Int] where
  toList :: [Int] -> [Int]
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList :: Tree Int -> [Int]
  toList Empty = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

sumL :: (Listable a) => a -> Int
sumL x = sum (toList x)
