import Control.Monad.Instances
import Control.Monad.State

addStuff :: Int -> Int
addStuff = do
  a <- (+2)
  b <- (*10)
  return(a + b)


addStuff' x = let
  a = (*2) x
  b = (+10) x
  in a + b

fff = do
  a <- [1, 2, 3]
  b <- ['a', 'b']
  return (a, b)

fff' = [1, 2, 3] >>= \n -> ['a', 'b'] >>= \ch -> return(n, ch)

fff'' = [(n, ch) | n <- [1,2,3], ch <- ['a', 'b']]


--threeCoins :: StdGen -> (Bool, Bool, Bool)
--threeCoins gen =
--  let (firstCoin, newGen) = random gen
--      (secondCoin, newGen') = random newGen
--      (thirdCon, newGen'') = random newGen'
--  in (firstCoin, secondCoin, thirdCoin)


type Stack = [Int]
pop' :: Stack -> (Int, Stack)
pop' (x:xs) = (x, xs)
push' ::Int -> Stack -> ((), Stack)
push' a xs = ((), a:xs)

stackManip' :: Stack -> (Int, Stack)
stackManip' stack = let
  ((), newStack1) = push' 3 stack
  (a, newStack2) = pop' newStack1
  in pop' newStack2


pop :: State Stack Int
pop = state $ \(x:xs) -> (x, xs)
push :: Int -> State Stack ()
push a = state $ \xs -> ((), a:xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  pop
  pop


stackStuff :: State Stack ()
stackStuff = do
  a <- pop
  if a == 5
    then push 5
    else do
      push 3
      push 8


moreStack :: State Stack ()
moreStack = do
  a <- stackManip
  if a == 100
    then stackStuff
    else return ()

