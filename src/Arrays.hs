module Arrays where

import Data.Array


type ArrayLength = Int

createNewEmptyArray :: ArrayLength -> Array Int a
createNewEmptyArray = undefined


fromList :: [a] -> Array Int a
fromList xs = listArray (0, l - 1) xs
  where l = length xs
