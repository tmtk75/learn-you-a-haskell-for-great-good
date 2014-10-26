eitherRight = Right "boom" >>= \x -> return (x++"!") :: Either String String


