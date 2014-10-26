

monadRule1 = return 3 >>= (\x -> Just (x + 1000000))
monadRule1' = return "Wow" >>= (\x -> [x,x,x])

monadRule2 = Just "move on up" >>= return
monadRule2' = [1,2,3,4] >>= return
monadRule2'' = putStrLn "Wah!" >>= return
