module TreapTest where

import qualified Treap as T


import Test.Hspec
import Test.QuickCheck
import System.Random



main :: IO ()
main = hspec $
    describe "empty" $
      it "is a valid Treap" $
        verbose $ property $ \seed -> T.checkTreap (T.newStdGenTreap seed)



