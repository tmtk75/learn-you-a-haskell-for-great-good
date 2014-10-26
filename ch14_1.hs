import Data.Monoid
import Control.Monad.Writer
--isBigGang :: Int -> Bool
--isBigGang x = x > 9

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

--applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)


type Food = String
type Price = Sum Int
addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)



rw = runWriter (return 3 :: Writer String Int)
rw' = runWriter (return 3 :: Writer (Sum Int) Int)
rw'' = runWriter (return 3 :: Writer (Product Int) Int)


logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  return (a*b)
rwmwl = runWriter multWithLog


multWithLog' = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"]
  return (a*b)
rwmwl' = runWriter multWithLog'


gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0    = a
  | otherwise = gcd' b (a `mod` b)

gcd'' :: Int -> Int -> Writer [String] Int
gcd'' a b
  | b == 0    = do
                tell ["Finished with " ++ show a]
                return a
  | otherwise = do
                tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
                gcd'' b (a `mod` b)

rgcd'' = fst $ runWriter (gcd'' 24 16)


gcdReverse a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      result <- gcdReverse b (a `mod` b)
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      return result


newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }
toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []


instance Monoid (DiffList a) where
  mempty = DiffList (\xs -> [] ++ xs)
  (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))



gcd''' :: Int -> Int -> Writer (DiffList String) Int
gcd''' a b
  | b == 0    = do
                tell (toDiffList ["Finished with " ++ show a])
                return a
  | otherwise = do
                tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
                gcd''' b (a `mod` b)


finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown x = do
  finalCountDown (x - 1)
  tell (toDiffList [show x])


finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
  tell ["0"]
finalCountDown' x = do
  finalCountDown' (x - 1)
  tell [show x]

