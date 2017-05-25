module Golf where

-- skips :: [a] -> [[a]]

localMaxima :: [Integer] -> [Integer]
localMaxima (n:m:o:xs)
  | m > n && m > o = m:(localMaxima $ m:o:xs)
  | otherwise = localMaxima $ m:o:xs
localMaxima _ = []
