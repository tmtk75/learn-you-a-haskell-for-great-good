{-# OPTIONS -Wall -Werror #-}
f :: Char -> Int
f 'a' = 0
f 'b' = 1
f x = read ([x] ++ "0") ::Int


firstLetter :: String -> String
firstLetter s@(x:_) = "The first letter is " ++ [x] ++ " in " ++ s
firstLetter [] = "empty stirng was given"


bmiTell :: Double -> Double -> String
bmiTell w h
  | bmi <= skinny = "You're underweight"
  | bmi <= normal = "You're normal"
  | bmi <= fat    = "You're fat"
  | otherwise     = "You're a whale"
  where bmi = w / (h*h)
        (skinny, normal, fat) = (18.5, 25.0, 30.0)


g :: Int
g = (let a = 2 in a * a)


localFunction :: Int -> [Int]
localFunction z = (let yy :: Int -> Int; yy y = y + 2 in [yy z])


splitTuple :: (Int,Int,Int) -> Int
splitTuple x = let (a,b,c) = x in a + b + c
