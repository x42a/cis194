import Data.Text.Lazy.Read (double)

-- Validating Credit Card Numbers
-- 1. Double every second digit from the right.
-- 2. Add the doubled and undoubled digits.
-- 3. Get the reminder of dividing the above sum by 10. If the result is 0, the
--    credit card is valid.

-- Convert a positive Integer to a list of its digits.
toDigits :: Integer -> [Integer]
-- Prelude Implementation
-- toDigits n
--   | n <= 0 = []
--   | otherwise = map (\c -> read [c] :: Integer) (show n)
toDigits n
  | n <= 0 = []
  | otherwise = toDigits next ++ [digit]
  where
    next = floor (fromInteger n / 10)
    digit = n - (next * 10)

-- Same as toDigits, but returns the digits in reversed order.
toDigitsRev :: Integer -> [Integer]
-- Prelude Implementation
-- toDigitsRev n = reverse (toDigits n)
toDigitsRev n = revIntegerList (toDigits n)

-- Util fn to reverse a list of integers
revIntegerList :: [Integer] -> [Integer]
revIntegerList [] = []
revIntegerList (x : xs) = revIntegerList xs ++ [x]

-- Doubles every second digit from the right.
doubleEveryOther :: [Integer] -> [Integer]
-- Prelude Implementation
-- doubleEveryOther l =
--   reverse (zipWith (curry doubleIfEvenIndex) (reverse l) [1 ..])
--
-- doubleIfEvenIndex :: (Integer, Integer) -> Integer
-- doubleIfEvenIndex (n, i)
--   | even i = n *
--   | otherwise = n
doubleEveryOther ds =
  revIntegerList (doubleEverySecond (revIntegerList ds) False)

doubleEverySecond :: [Integer] -> Bool -> [Integer]
doubleEverySecond [] _ = []
doubleEverySecond (x : xs) shouldDbl =
  doubleValueWhen x shouldDbl : doubleEverySecond xs (not shouldDbl)

doubleValueWhen :: Integer -> Bool -> Integer
doubleValueWhen n True = n * 2
doubleValueWhen n False = n

-- Sum digits from a list
sumDigits :: [Integer] -> Integer
-- Prelude Way
-- sumDigits [] = 0
-- sumDigits (x : xs) = sum (toDigits x) + sumDigits xs
sumDigits [] = 0
sumDigits (x : xs)
  | x > 9 = sumDigits (toDigits x) + sumDigits xs
  | otherwise = x + sumDigits xs

-- Validate credit cards using the algorithm above
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
