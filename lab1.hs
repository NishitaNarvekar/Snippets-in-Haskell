-- The following function should take a list of Integers
-- and return the largest.
-- THE DEFINITION MUST BE RECURSIVE.
maxNum :: [Integer] -> Integer
maxNum [] = 0

maxNum (x:xs) =
  if(x > maxNum xs)
    then x
    else
      maxNum xs

-- Do the game fizzbuzz (http://en.wikipedia.org/wiki/Fizz_buzz).
-- Return a string counting from 1 to the specified number.
-- Replace numbers divisible by 3 with "fizz" and numbers divisible
-- by 5 with "buzz".  If a number is divisible by both 3 and 5,
-- replace it with "fizzbuzz".
fizzbuzz :: Int -> String
x=""


fizzbuzz n =
  if(n == 0)
    then ""
    else if(n `mod` 3 == 0 && n `mod` 5 == 0)
      then fizzbuzz(n-1) ++ x ++ " fizzbuzz "
      else if(n `mod` 5 == 0)
        then fizzbuzz(n-1) ++ x ++ " buzz "
        else if(n `mod` 3 == 0 )
        then fizzbuzz(n-1)++ x ++ " fizz "
          else
            --c <- n
            fizzbuzz(n-1) ++ show n ++ " " 
