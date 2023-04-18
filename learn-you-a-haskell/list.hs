{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

boom_bangs :: [Int] -> [String]
boom_bangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

not_13_15_19 = [x | x <- [10 .. 20], x /= 13, x /= 15, x /= 19]

-- big_res_only [1..10] [1..10]
-- [54,60,56,63,70,56,64,72,80,54,63,72,81,90,60,70,80,90,100]
big_res_only :: (Num a, Ord a) => [a] -> [a] -> [a]
big_res_only xs ys = [x * y | x <- xs, y <- ys, x * y > 50]

only_evens :: (Integral a) => [[a]] -> [[a]]
only_evens xxs = [[x | x <- xs, even x] | xs <- xxs]

flat_list :: (Integral a) => [[a]] -> [a]
flat_list xxs = [x | xs <- xxs, x <- xs]