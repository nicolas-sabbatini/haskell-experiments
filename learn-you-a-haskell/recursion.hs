{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

fib :: (Integral a) => a -> a
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib x = fib (x - 1) + fib (x - 2)

fib_perf :: (Integral a) => a -> a
fib_perf x = fib_rec x 0 1
  where
    fib_rec :: (Integral a) => a -> a -> a -> a
    fib_rec 0 a _ = a
    fib_rec 1 _ current = current
    fib_rec i prev current = fib_rec (i - 1) current (prev + current)

list_max :: (Ord a) => [a] -> a
list_max [] = error "Empty list"
list_max [x] = x
list_max (x : xs) = max x (list_max xs)

left_pad :: Int -> Char -> String -> String
left_pad 0 _ s = s
left_pad x pad s
  | x <= length s = s
  | otherwise = left_pad x pad (pad : s)

quick_sort :: (Ord a) => [a] -> [a]
quick_sort [] = []
quick_sort (pivot : xs) =
  let smaller = quick_sort [a | a <- xs, a <= pivot]
      bigger = quick_sort [a | a <- xs, a > pivot]
   in smaller ++ [pivot] ++ bigger
