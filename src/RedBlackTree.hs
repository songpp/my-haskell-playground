module RedBlackTree where

data Color = Red | Black deriving (Show, Eq)
data RB k = Leaf | T k Color (RB k) (RB k) deriving (Show, Eq)

empty :: RB k
empty = Leaf

singleton :: RB k
singleton k = T k Red Leaf Leaf

contains :: Ord k => k -> RB k -> Bool
contains k Leaf = False
contains k (T k1 _ l r)
    | k <  k1 = contains k l
    | k == k1 = True
    | k >  k1 = contains k r


insert :: Ord k => k -> RB k -> RB k
insert k Leaf = singleton k
insert k tree = T k' Black l r
  where
    (T k' _ l r) = go tree
    go t@(T k1 color left right)
      | k <  k1 = balance k1 color (go left) right
      | k == k1 = t
      | k >  k1 = balance k1 color left (go right)

