{-# OPTIONS -Wall -Werror #-}
max' :: (Ord a) => [a] -> a
max' [] = error "no answer"
max' [x] = x
max' (x:xs) = max x (max' xs)


rep' :: Int -> a -> [a]
rep' n a
  | n <= 0    = [] 
  | otherwise = a:rep' (n - 1) a


tk' :: Int -> [a] -> [a]
tk' n a
  | n <= 0    = []
  | otherwise = (head a) : (tk' (n - 1) (tail a))
