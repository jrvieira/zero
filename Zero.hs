{-# LANGUAGE BangPatterns #-}

module Zero.Zero where

import Zero.Color
-- import Zero.Draw
import Debug.Trace
import Data.List
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

echo :: Show a => String -> a -> IO ()
echo s x = do
   putStr $ clr Cyan $ s <> ": "
   print x
   putStrLn []

-- trace
infix 1 #
(#) :: a -> String -> a
(#) a s = trace (clr Bold $ clr Blue $ "# " ++ s) a

-- parse numbers from a string
parseNums :: (Read a,Num a) => String -> [a]
parseNums = go [] []
   where
   go nums acc ""
      | null acc = (read . reverse) <$> reverse nums
      | otherwise = (read . reverse) <$> reverse (acc:nums)
   go nums acc (c:cs)
      | isDigit c = go nums (c:acc) cs
      | null acc  = go nums acc cs
      | otherwise = go (acc:nums) [] cs

-- remove duplicates
unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

-- last n elements
takeLast :: Int -> [a] -> [a]
takeLast n l = go (drop n l) l
   where
   go [] rs = rs
   go xs ys = go (tail xs) (tail ys)

-- fixed point of iteration
fixp :: Eq a => (a -> a) -> a -> a
fixp f !a
   | a == a' = a
   | otherwise = fixp f a'
   where
   a' = f a

-- remove element from assoc by key 
remove :: Eq k => k -> [(k,v)] -> [(k,v)]
remove _ [] = []
remove k (x:xs)
   | k == fst x = remove k xs
   | otherwise = x : remove k xs

-- remove element from assoc by value 
erase :: Eq v => v -> [(k,v)] -> [(k,v)]
erase _ [] = []
erase v (x:xs)
   | v == snd x = erase v xs
   | otherwise = x : erase v xs

-- tortoise and hare cycle finding
floyd :: Eq a => [a] -> Maybe (Integer,a)
floyd [] = Nothing
floyd (a:as) = go 1 1 a as
   where
   go _ _ _ [] = Nothing
   go pow lam x (y:ys)
      | x == y     = Just (lam,x)
      | pow == lam = go (2*pow) 1 y ys
      | otherwise  = go pow (1+lam) x ys

-- simple plot from list of ints
-- plot :: String -> [Int] -> IO ()
-- plot f = draw f . go 0 
--    where
--    go _ [] = []
--    go n (x:xs) = [((n,y),True) | y <- [0..x]] ++ go (succ n) xs

-- test
test :: Show a => String -> (a -> Bool) -> a -> IO ()
test t p a = putStrLn $ unwords ['\n' : t , clr c m , '\n' : clr c (show a) , "\n"]
   where
   (c,m)
      | p a = (Green,"v")
      | otherwise = (Red,"x")

teqt :: (Eq a,Show a) => String -> a -> a -> IO ()
teqt t e a = putStrLn $ unwords ['\n' : t , clr c m , r , '\n' : clr c (show a) , "\n"]
   where
   (c,m,r)
      | a == e = (Green,"v","")
      | otherwise = (Red,"x",'\n' : show e)

-- set
(∈) :: (Foldable t, Eq a) => a -> t a -> Bool
(∈) = elem

(∉) :: (Foldable t, Eq a) => a -> t a -> Bool
(∉) e = not . elem e

(∋) :: (Foldable t, Eq a) => t a -> a -> Bool
(∋) = flip elem

class Comparable s where
   (∪) :: s -> s -> s
   (∩) :: s -> s -> s

instance Eq a => Comparable [a] where
   (∪) = union
   (∩) = intersect

instance Ord a => Comparable (Set a) where
   (∪) = Set.union
   (∩) = Set.intersection

-- queue
data Queue a = Queue [a] [a]

instance Semigroup (Queue a) where
   Queue ia oa <> Queue ib ob = Queue (ib <> reverse ob) (oa <> reverse ia)

instance Monoid (Queue a) where
   mempty = Queue [] []

instance Functor Queue where
   fmap f (Queue i o) = Queue (fmap f i) (fmap f o)

instance Foldable Queue where
   foldr f z (Queue i o) = foldr f (foldr f z o) (reverse i)

queue :: [a] -> Queue a
queue = Queue []

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue i o) = Queue (x:i) o

dequeue :: Queue a -> (Maybe a,Queue a)
dequeue q@(Queue i o)
   | null q = (Nothing,q)
   | null o = dequeue $ Queue [] (reverse i)
   | otherwise = (Just $ head o,Queue i (tail o))

