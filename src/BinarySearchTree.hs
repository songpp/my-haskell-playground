{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
module BinarySearchTree where

import Control.Monad (void)
import Data.Maybe (isJust)

-- | A Binary Search Tree
data BSTree k v where
  Nil :: BSTree k a
  Node :: !k -> v -> BSTree k v -> BSTree k v -> BSTree k v
  deriving (Show, Eq, Ord)

empty :: Ord k => BSTree k v
empty = Nil

singleton :: Ord k => k -> v -> BSTree k v
singleton k a = Node k a Nil Nil

insert :: Ord k =>  k -> v -> BSTree k v-> BSTree k v
insert k a Nil = singleton k a
insert k a (Node !k1 v l r)
  | k == k1   = Node k a l r
  | k >  k1   = Node k1 v l (insert k a r)
  | otherwise =  Node k1 v (insert k a l) r

contains :: Ord k => k -> BSTree k v -> Bool
contains k = isJust . search k

search :: Ord k => k -> BSTree k v -> Maybe v
search k Nil = Nothing
search k (Node k1 v l r)
  | k == k1   = Just v
  | k <  k1   = search k l
  | otherwise = search k r

minNode :: Ord k => BSTree k v -> BSTree k v
minNode Nil = Nil
minNode t@(Node _ _ l _) = case l of
  Nil -> t
  otherwise -> minNode l


minKey :: Ord k => BSTree k v -> Maybe (k, v)
minKey Nil = Nothing
minKey (Node k v Nil r) = Just (k,v)
minKey (Node _ _ l _)   = minKey l

maxKey :: Ord k => BSTree k v -> Maybe (k, v)
maxKey Nil = Nothing
maxKey (Node k v _ Nil) = Just (k,v)
maxKey (Node _ _ _ r) = maxKey r


floorKey :: Ord k => k -> BSTree k v -> Maybe (k, v)
floorKey k Nil = Nothing
floorKey k t = go k t Nothing
  where
    go k Nil p = p
    go k n@(Node k1 v l r) p
      | k < k1 = p
      | k >= k1 = go k r (Just (k1, v))

ceilingKey :: Ord k => k -> BSTree k v-> Maybe (k, v)
ceilingKey k Nil = Nothing
ceilingKey k t = go k t Nothing
  where
    go k Nil p = p
    go k n@(Node k1 v l r) p
      | k1 < k = go k r Nothing
      | k1 >= k = Just (k1, v)

deleteMinKey :: Ord k => BSTree k v -> BSTree k v
deleteMinKey Nil = Nil
deleteMinKey (Node k v Nil r) = r
deleteMinKey (Node k v l r)  = Node k v (deleteMinKey l) r


deleteKey  :: Ord k => k -> BSTree k v -> BSTree k v
deleteKey k Nil = Nil
deleteKey k (Node k1 v Nil Nil) = Nil
deleteKey k t@(Node k1 v Nil r)
  | k == k1 = r
  | k < k1  = t
  | otherwise = Node k1 v Nil (deleteKey k r)
deleteKey k t@(Node k1 v l Nil)
  | k == k1 = l
  | k > k1  = t
  | otherwise = Node k1 v (deleteKey k l) Nil
deleteKey k (Node k1 v l r)
  | k == k1 = let Node k' v' _ _ = minNode r in Node k' v' l (deleteMinKey r)
  | k > k1  = Node k1 v l (deleteKey k r)
  | otherwise = Node k1 v (deleteKey k l) r


-- inorder
keys :: Monad m => BSTree k v -> (k -> m b) -> m [b]
keys Nil f = return []
keys (Node k v l r) f = do
  ls <- keys l f
  x <- f k
  rs <- keys r f
  return (ls ++ (x : rs))

instance Foldable (BSTree k) where
  foldr f b Nil = b
  foldr f b (Node k v l r) = f v (foldr f (foldr f b r) l)

instance Functor (BSTree k) where
  fmap f Nil = Nil
  fmap f (Node k v l r) = Node k (f v) (fmap f l) (fmap f r)

instance Traversable (BSTree k) where
  traverse f Nil = pure Nil
  traverse f (Node k v l r) = Node k <$> f v <*> traverse f l <*> traverse f r


--
--
--

printKeys :: (Show k) => BSTree k v -> IO ()
printKeys t = void (keys t print)

t = foldl (\t b -> insert b b t) Nil ([100, 90..10] ++ [13, 21..99])
-- printKeys t

newtype Sum = Sum Integer deriving (Show, Eq, Ord)

instance Monoid Sum where
  mempty = Sum 0
  mappend (Sum a) (Sum b) = Sum (a + b)

newtype Product = Product Integer deriving (Show, Eq, Ord)

instance Monoid Product where
  mempty = Product 1
  mappend (Product x) (Product y) = Product (x * y)
