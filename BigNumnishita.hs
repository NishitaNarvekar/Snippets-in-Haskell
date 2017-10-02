{-
  Name: Nishita Narvekar
  Class: CS 252
  Assigment: HW1
  Date: <Date assignment is due>
  Description: <Describe the program and what it does>
-}

module BigNum (
  BigNum,
  bigAdd,
  bigSubtract,
  bigMultiply,
  bigEq,
  bigDec,
  bigPowerOf,
  prettyPrint,
  stringToBigNum,
) where

type Block = Int -- An Int from 0-999

type BigNum = [Block]

maxblock = 1000

bigAdd :: BigNum -> BigNum -> BigNum
bigAdd x y =
  if(length x /= length y && length y < length x)
    then do
      let y' = addLeadingZeroes y (length x - length y)
        in bigAdd' x y' 0
      else if (length x /= length y && length x < length y)
        then  do
          let x'= addLeadingZeroes x (length y - length x)
            in bigAdd' x' y 0
        else
          bigAdd' x y 0





bigAdd' :: BigNum -> BigNum -> Block -> BigNum

bigAdd' (x:xs) (y:ys) z = if(length xs == 0 && length ys == 0 && (x+y+z) `div` 1000 == 0)
                            then [x+y+z]
                              else if(length xs == 0 && length ys == 0 && (x+y+z) `div` 1000 /= 0)
                                then [(x+y+z) `mod` 1000] ++ [((x+y+z) `div` 1000)]
                                  else if((x+y+z) `div` 1000 < 0)
                                    then [x+y+z] ++ bigAdd' xs ys 0
                                      else
                                        [(x+y+z) `mod` 1000] ++ bigAdd' xs ys ((x+y+z) `div` 1000)



bigSubtract :: BigNum -> BigNum -> BigNum
bigSubtract x y =
  if length x < length y
    then error "Negative numbers not supported"
    else reverse $ stripLeadingZeroes $ reverse result
      where result = bigSubtract'' x y

bigSubtract'' :: BigNum -> BigNum -> BigNum
bigSubtract'' x y =
  if(length x /= length y && length x > length y)
    then do
      let y' = addLeadingZeroes y (length x - length y)
        in bigSubtract' x y' 0
      else if (length x /= length y && length x > length y)
        then  do
          let x'= addLeadingZeroes x (length y - length x)
            in bigSubtract' x' y 0
        else
          bigSubtract' x y 0



stripLeadingZeroes :: BigNum -> BigNum
stripLeadingZeroes (0:[]) = [0]
stripLeadingZeroes (0:xs) = stripLeadingZeroes xs
stripLeadingZeroes xs = xs

addLeadingZeroes :: BigNum -> Int -> BigNum
addLeadingZeroes x n =
  if(n == 0)
    then x
      else
        addLeadingZeroes x (n-1) ++ [000]






-- Negative numbers are not supported, so you may throw an error in these cases
bigSubtract' :: BigNum -> BigNum -> Block -> BigNum
bigSubtract' (x:xs) (y:ys) z = if(length xs == 0 && length ys == 0)
                            then [x-y-z]
                              else if((x-y-z) < 0)
                                then [x-y-z+1000] ++ bigSubtract' xs ys 1
                                  else
                                    [(x-y-z) `mod` 1000] ++ bigSubtract' xs ys 0

bigEq :: BigNum -> BigNum -> Bool
bigEq (x:xs) (y:ys) = if(length xs == 0 && length ys == 0 && x==y)
                        then True
                          else if(x == y)
                                then bigEq xs ys
                            else
                              False

bigDec :: BigNum -> BigNum
bigDec x = bigSubtract x [1]

bigMultiply :: BigNum -> BigNum -> BigNum
bigMultiply (x:xs) y = if(head y == 0)
                          then [0]
                            else if(length xs == 0)
                              then [x * head y]
                                else
                                  [x * head y] ++ bigMultiply xs y

bigPowerOf :: BigNum -> BigNum -> BigNum
bigPowerOf (x:xs) y = if(head y == 0)
                          then [1]
                            else if(length xs == 0)
                              then [x ^ head y]
                                else
                                  [x ^ head y] ++ bigPowerOf xs y

prettyPrint :: BigNum -> String
prettyPrint [] = ""
prettyPrint xs = show first ++ prettyPrint' rest
  where (first:rest) = reverse xs

prettyPrint' :: BigNum -> String
prettyPrint' [] = ""
prettyPrint' (x:xs) = prettyPrintBlock x ++ prettyPrint' xs

prettyPrintBlock :: Int -> String
prettyPrintBlock x | x < 10     = ",00" ++ show x
                   | x < 100    = ",0" ++ show x
                   | otherwise  = "," ++ show x

stringToBigNum :: String -> BigNum
stringToBigNum "0" = [0]
stringToBigNum s = stringToBigNum' $ reverse s

stringToBigNum' :: String -> BigNum
stringToBigNum' [] = []
stringToBigNum' s | length s <= 3 = read (reverse s) : []
stringToBigNum' (a:b:c:rest) = block : stringToBigNum' rest
  where block = read $ c:b:a:[]
