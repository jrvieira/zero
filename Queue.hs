module Zero.Queue where


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

