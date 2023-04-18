{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- Estoy seguro de que esto se puede hacer mejor
fizz_buzz :: Int -> String
fizz_buzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 3 == 0 = "Fizz"
  | n `mod` 5 == 0 = "Buzz"
  | otherwise = show n

fizz_buzz_v2 :: Int -> String
fizz_buzz_v2 n = if null res then show n else res
  where
    fizz_v2 n = if n `mod` 3 == 0 then "Fizz" else ""
    buzz_v2 n = if n `mod` 5 == 0 then "Buzz" else ""
    res = fizz_v2 n ++ buzz_v2 n