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
