doubleUs x y = doubleMe x  + doubleMe y

doubleMe x = x + x

doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

conanO'Brien = "It's a-me, Conan O'Brien!"

length' x = sum [1 | _ <- x]

