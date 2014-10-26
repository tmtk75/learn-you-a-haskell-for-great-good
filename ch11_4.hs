import Control.Applicative
import Data.Monoid
import qualified Data.Foldable as F


infixl 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node a (treeInsert x left) right
  | x > a  = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a  = treeElem x left
  | x > a  = treeElem x right

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

aaaaa :: Int -> Int
aaaaa x = 1


data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Eq, Ord, Show)


sequenceA :: (Applicative f) => [f a] -> f [a]
--sequenceA [] = pure []
--sequenceA (x:xs) = (:) <$> x <*> sequenceA xs
sequenceA = foldr (liftA2 (:)) (pure [])



newtype Pair b a = Pair { getPair :: (a, b) }
instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)



--data CoolBool = CoolBool { getCoolBool :: Bool }
newtype CoolBool = CoolBool { getCoolBool :: Bool }
helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"



instance F.Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                           f x           `mappend`
                           F.foldMap f r



applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

