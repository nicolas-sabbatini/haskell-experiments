{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

duble_it :: Num a => a -> a
duble_it x = x + x

duble_us :: Num a => a -> a -> a
duble_us x y = duble_it x + duble_it y

duble_if_small :: (Num a, Ord a) => a -> a
duble_if_small x = if x > 100 then x else duble_it x

-- Comprencion de listas
mod_7 = [x | x <- [1 .. 100], x `mod` 7 == 0]

duble_0_100 = [duble_it x | x <- [1 .. 100]]
