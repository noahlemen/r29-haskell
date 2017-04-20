{-
  Exercise 01:
  `toDigits` should convert positive Integers
  to a list of digits. (For 0 or negative
  inputs, toDigits should return the empty
  list.)

  `toDigitsRev` should do the same, but with
  the digits reversed.

  Example:
  toDigits 1234 == [1,2,3,4]
  toDigitsRev 1234 == [4,3,2,1]
  toDigits 0 == []
  toDigits (-17) == []
-}

toDigits      :: Integer -> [Integer]
toDigitsRev   :: Integer -> [Integer]

toDigits n
  | n > 0     = [read [x] :: Integer | x <- show n]
  | otherwise = []

toDigitsRev m = reverse (toDigits m)

{-
  Exercise 02:
  Once we have the digits in the proper order, we need to
  double every other one.

  Remember that doubleEveryOther should double every other
  number beginning from the right, that is, the second-to-last,
  fourth-to-last,...numbers are doubled.

  Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
  Example: doubleEveryOther [1,2,3] == [1,4,3]
-}

doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther ints = [
  if (((length ints) - index) `mod` 2) == 0
  then int*2
  else int
  | (int, index) <- zip ints [0..]]

{-
  Exercise 03:
  The output of doubleEveryOther has a mix of one-digit
  and two-digit numbers. Define the function to calculate
  the sum of all digits.

  Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
-}

sumDigits :: [Integer] -> Integer

sumDigits list = sum [ sum (toDigits i) | i <- list ]

{-
  Exercise 04:
  Define the function that indicates whether an Integer
  could be a valid credit card number. This will use all
  functions defined in the previous exercises.

  Example: validate 4012888888881881 = True
  Example: validate 4012888888881882 = False
-}

validate :: Integer -> Bool

validate num = sumDigits (doubleEveryOther (toDigits num)) `mod` 10 == 0
