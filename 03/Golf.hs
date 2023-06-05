module Golf where

{-
The skips function takes a list of any type, xs :: [a].

The skips function returns a list of lists, which length is that of the input
list and which type is [[a]].

The first element of the output must be the input itself.

From then on, every nth list of the output contains every nth element of the
input. For instance, the second list of the output must contain every second
element of the input, the third every third, etc.:

> skips "ABCD" == ["ABCD","BD","C","D"]

First of all, we must add the input, xs, at the start of the output as we
mentioned before.

Then, we create a list containing the index of each remaining list of the
output. Because we have already added xs, we go from 2 up to the length of xs
(which is also the total length of the output).

Next, we map this list of indexes.

Given that we know the index of every list, we must get every element of the
input that satisfies: "j `mod` i == 0" (i being the index of the output list we
are working on and j the index of an element of the input). When the module of
two numbers is 0, that means that the first number is divisible by the second,
therefore being a nth element of the nth list.

To achieve this, we zip together (join) xs and [1..], so that every element
of xs is paired with its index. Then, that list of touples is filtered using the
above equation.

Finally, the filtered list of touples must be mapped with fst, which returns the
first element of a touple, the nth elements in this case.
 -}
skips :: [a] -> [[a]]
skips [] = []
{- longer, but more efficient way
skips xs =
  xs
    : map
      (\i -> map fst $ filter (\(_, j) -> j `mod` i == 0) xsIndexed)
      [2 .. (length xs)]
  where
    xsIndexed = zip xs [1 ..] -}
skips xs =
  xs
    : map
      (\i -> map fst $ filter (\(_, j) -> mod j i == 0) (zip xs [1 ..]))
      [2 .. length xs]

{-
The localMaxima function takes a list of integers and returns a new list
containing each local maximum, an element greater than the element immediately
before and after.

First of all, we generate a list of the possible local maxima indexes:
[1 .. (length xs - 2)], which starts at the second element (index 1) and ends at
the second-to-last element (the length of xs is 1 above the last index, so the
length minus 2 is the one we are after).

The first and last elements can never be a local maximum, then are not included.

Now we map that list to get each possible local maximum (LM) and its adjacent
elements (previous and next). In order to do that, we remove the first i - 1
elements of xs (i is the index of the possible LM and we substruct 1 to get also
the previous value. Then, we take 3 from it, which creates a three element list,
the possible LM in the middle.

The new list is filtered using a three element patern and checking that in fact,
the element in the middle is greater than the previous and next.

Finally, we map the three elements lists to extract the middle one, the LMs.
 -}
localMaxima :: [Integer] -> [Integer]
localMaxima xs =
  map (\(_ : y : _) -> y) $
    filter (\(x : y : z : _) -> x < y && y > z) $
      map (\i -> take 3 (drop (i - 1) xs)) [1 .. (length xs - 2)]

{-
The historgram function takes a list of integers and produces a histogram string
like this one:
> histogram [1,4,5,4,6,6,3,4,2,4,9]
    *
    *
    * *
 ******  *
==========
0123456789

Basically, the input list contains numbers from 0 to 9 and the program has to
count how many times each one appears to create then the output string.

First of all, we add the bottom of the historam, which never changes
("==========\n0123456789\n"). After that we must count the appearence of each
number. In order to do that, we map a list of ints from 0 to 9 (the possible
inputs in order) by filtering with an equal to (==). That list then is applied
to length get the count.

Then, we call the b function, which is recursive. b constructs each count-line
of the histogram. To do that, first it checks that at least a count is still
greater than 0 to generate a line (all (<=0) ys).

Otherwise, it will call itself with ys substracting 1 to each count, so that the
next line is generated. After that, because the lines of greater counts come 
first, it constructs the line by checking that a count is in fact greater than 0
If so, it puts an *, else a whitespace. After each line a line break (\n) is 
added.  
 -}
histogram :: [Integer] -> String
histogram xs =
  b (map (\i -> length $ filter (== i) xs) [0 .. 9])
    ++ "==========\n0123456789\n"
  where
    b ys | all (<= 0) ys = ""
    b ys =
      b (map (+ (-1)) ys)
        ++ (map (\n -> if n > 0 then '*' else ' ') ys ++ "\n")
