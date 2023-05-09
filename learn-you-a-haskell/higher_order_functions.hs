{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use map" #-}

hundred_is :: (Ord a, Num a) => a -> Ordering
hundred_is = compare 100

div_by_10 :: (Floating a) => a -> a
div_by_10 = (/ 10)

max_or_100 :: (Ord a, Num a) => a -> a
max_or_100 = max 100

is_uppercase :: Char -> Bool
is_uppercase = (`elem` ['A' .. 'Z'])

-- Can't use (- 10) because it interpret the expresion as a negative number
subtract_10 :: (Num a) => a -> a
subtract_10 = subtract 10

apply_twice :: (a -> a) -> a -> a
apply_twice fun x = fun (fun x)

zip_map :: (a -> b -> c) -> [a] -> [b] -> [c]
zip_map _ [] _ = []
zip_map _ _ [] = []
zip_map fun (x : xs) (y : ys) = fun x y : zip_map fun xs ys

largest_divisible :: (Integral a) => a -> a -> a
largest_divisible list_start target = head (filter can_div [list_start, list_start - 1 ..])
  where
    can_div n = n `mod` target == 0

sum_odd_squares_till :: (Integral a) => a -> a
sum_odd_squares_till limit = sum (takeWhile (< limit) (filter odd (map (^ 2) [1 ..])))

-- With lambdas: length ( filter (\ xs -> length xs < 10) (map chain [1..1000]))
-- With sections: length (let g xs = length xs < 10 in filter g (map chain [1..1000]))
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

-- lambdas can have multiple arguments: zipWith (\a b -> (a * 32 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
-- Lambdas can pattern match: map (\(a, b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]

elem_fold :: (Eq a) => a -> [a] -> Bool
elem_fold y = foldl (\acc x -> (x == y) || acc) False

map_foldr :: (a -> b) -> [a] -> [b]
map_foldr f = foldr (\x acc -> f x : acc) []
