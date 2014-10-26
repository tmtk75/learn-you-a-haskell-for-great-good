foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))


bar :: Maybe String
bar = do
      x <- Just 3
      y <- Just "!"
      Just (show x ++ y)

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs((left + n) - right) < 4 = Just (left + n, right)
  | otherwise                   = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs(left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                   = Nothing

x -: f = f x

banana :: Pole -> Maybe Pole
banana _ = Nothing

routine = do
  start <- return (0, 0)
  first <- landLeft 2 start
  Nothing
  second <- landRight 2 first
  landLeft 1 second


justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x


wopwop :: Maybe Char
wopwop = do
    (x:xs) <- Just ""
    return x


