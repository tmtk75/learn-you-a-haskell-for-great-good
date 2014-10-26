import Control.Monad.Instances
import System.Random
import Control.Monad.State

type Stack = [Int]

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1,2,3]
    then put [8,3,1]
    else put [9,2,1]


pop :: State Stack Int
pop = do
  (x:xs) <- get
  put xs
  return x


push x = do
  xs <- get
  put (x:xs)


randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins :: State StdGen (Bool, Bool, Bool)
threeCoins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)


