module Arrays where

import Data.Array.IArray


type ArrayLength = Int

createNewEmptyArray :: ArrayLength -> Array Int a
createNewEmptyArray = undefined


fromList :: [a] -> Array Int a
fromList xs = listArray (0, l - 1) xs
  where l = length xs


buildPair :: (Int, Int)
buildPair = let arr  = listArray (1,10) (repeat 37) :: Array Int Int
                arr' = arr // [(1, 64)]
            in (arr ! 1, arr' ! 1)
