greaterThan100_2 :: [Integer] -> [Integer]
greaterThan100_2 = filter (> 100)

foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g x = f (g x)

f :: Int -> Int -> Int
f x y = 2 * x + y

foobar :: [Integer] -> Integer
foobar [] = 0
foobar (x : xs)
  | x > 3 = (7 * x + 2) + foobar xs
  | otherwise = foobar xs

foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7 * x + 2) . filter (> 3)
