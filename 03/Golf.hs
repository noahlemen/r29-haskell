module Golf where

skips :: [a] -> [[a]]
skips a = map (\i -> everyNth a i) [i | (_, i) <- zip a [1..] ]

everyNth :: [b] -> Integer -> [b]
everyNth b n = map fst $ filter (\(_, i) -> i `mod` n == 0) $ [(el, i) | (el, i) <- zip b [1..] ]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | y > x && y > z = y:(localMaxima $ y:z:xs)
  | otherwise = localMaxima $ y:z:xs
localMaxima _ = []
