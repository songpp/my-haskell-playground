{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module Treap where

import System.Random

data Treap p k where
  Nil :: Treap p k
  Br  :: (Ord k, Ord p)
      => p           -- priority
      -> k           -- key
      -> Treap p k   -- left child
      -> Treap p k   -- right child
      -> Treap p k

priority :: Bounded p => Treap p k -> p
priority (Br p _ _ _) = p
priority Nil = minBound

treapContains :: (Ord k) => k -> Treap p k -> Bool
treapContains _   Nil = False
treapContains key (Br _ k l r)
    | key == k = True
    | key <  k = treapContains key l
    | key >  k = treapContains key r
treapContains _ _ = False

treapInsert :: (Ord p, Ord k, Bounded p) => p -> k -> Treap p k -> Treap p k
treapInsert p key Nil = Br p key Nil Nil
treapInsert p key t@(Br p' k' left right)
    | key < k' = rotate $ Br p' k' (treapInsert p key left) right
    | key > k' = rotate $ Br p' k' left (treapInsert p key right)
    | otherwise = t
  where
    rotate Nil = Nil
    rotate tree@(Br pri _ l r)
      -- ‘priority’ function uses minBounds for 'Nil's,
      --  so we can always guarantee here 'l' and 'r' are 'Br's
      | pri < priority l = rotateRight id tree
      | pri < priority r = rotateLeft  id tree
      | otherwise        = tree

treapDelete :: Ord k => k -> Treap p k -> Treap p k
treapDelete _ Nil = Nil
treapDelete key t@(Br p k l r)
    | key > k = Br p k l (treapDelete key r)
    | key < k = Br p k (treapDelete key l) r
    | otherwise = rotateDown t
  where
    rotateDown Nil = Nil
    rotateDown (Br _ _ Nil Nil) = Nil
    rotateDown b@(Br _ _ Nil Br{}) = rotateLeft rotateDown b
    rotateDown b@(Br _ _ Br{} Nil) = rotateRight rotateDown b
    rotateDown b@(Br{}) = rotateRight rotateDown b

rotateRight , rotateLeft:: (Treap p k -> Treap p k) -> Treap p k -> Treap p k
rotateRight f (Br p k (Br p' k' l' r') r) = Br p' k' l' (f (Br p k r' r))
rotateRight _ t = t
rotateLeft  f (Br p k l (Br p' k' l' r')) = Br p' k' (f (Br p k l l')) r'
rotateLeft  _ t = t

treapEmpty :: Treap p k
treapEmpty = Nil

newtype RandomTreap g p k = RT (g, Treap p k) deriving (Show)

deriving instance (Show a, Show p) => Show (Treap p a)
deriving instance (Eq a, Eq p) => Eq (Treap p a)

newStdGenTreap :: Int -> RandomTreap StdGen p k
newStdGenTreap seed = RT (mkStdGen seed, treapEmpty)

insert :: (RandomGen g, Random p, Ord k, Bounded p, Ord p)
       => k  -- key
       -> RandomTreap g p k
       -> RandomTreap g p k
insert a (RT (gen, treap)) = RT (g', treapInsert r a treap)
  where
    (r, g') = randomR (minBound, maxBound) gen

contains :: Ord k => k -> RandomTreap g p k -> Bool
contains key (RT (_ , treap)) = treapContains key treap

delete :: Ord k => k -> RandomTreap g p k-> RandomTreap g p k
delete _ t@(RT (_, Nil)) = t
delete a (RT (gen, treap)) = RT (gen, treapDelete a treap)

fromList :: (RandomGen g, Random p, Ord k, Bounded p, Ord p)
         => [k] -> RandomTreap g p k -> RandomTreap g p k
fromList keys rt = foldr insert rt keys

checkTreap :: RandomTreap g p k -> Bool
checkTreap (RT (_, t)) = check t
  where
    -- check whether Binary Search Tree and Heap protperties holds
    check Nil = True
    check (Br _ _ Nil Nil) = True
    check (Br p k c@(Br p' k' _ _) Nil) = k > k' && p >= p' && check c
    check (Br p k Nil c@(Br p' k' _ _)) = k < k' && p >= p' && check c
    check (Br p k l@(Br p1 k1 _ _) r@(Br p2 k2 _ _)) =
      k > k1 && k < k2 && p >= p1 && p >= p2 && check l && check r

-- let rt = newStdGenTreap 100 :: RandomTreap StdGen Int Int
-- let rt1 = fromList [100, 200, 900, 1000, 500, 712, 134] rt
-- checkTreap rt1
