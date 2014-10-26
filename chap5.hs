{-# OPTIONS -Wall -Werror #-}

diviedByTen :: (Floating a) => a -> a
diviedByTen = (/10)


applyTwice :: (a -> a) -> (a -> a)
applyTwice f x = f (f x)


zipWith' :: (t -> t1 -> a) -> [t] -> [t1] -> [a]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x  y : zipWith' f xs ys


-- sum10000square :: Integer
-- sum10000square = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))


rev' :: [a] -> [a]
rev' a = foldl (\acc x -> x:acc) [] a
