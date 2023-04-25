{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use foldr" #-}

-- Pattern matching
lucky_7 :: (Eq a, Num a) => a -> String
lucky_7 7 = "Lucky number 7!"
lucky_7 _ = "Good luck next time pal!"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

show_list :: (Show a) => [a] -> String
show_list [] = "End"
show_list (x : xs) = show x ++ show_list xs

let_fizz_buzz :: Int -> String
let_fizz_buzz num = "The answer is " ++ (let fizz n = if n `mod` 3 == 0 then "Fizz" else ""; buzz n = if n `mod` 5 == 0 then "Buzz" else ""; res n = if null (fizz n ++ buzz n) then show n else fizz n ++ buzz n in res num)

caseof_fizz_buzz :: Int -> String
caseof_fizz_buzz n =
  "The answer is " ++ case (n `mod` 3, n `mod` 5) of
    (0, 0) -> "FizzBuzz"
    (0, _) -> "Fizz"
    (_, 0) -> "Buzz"
    (_, _) -> show n
