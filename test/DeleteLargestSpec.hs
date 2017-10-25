module DeleteLargestSpec (spec) where

import Data.Foldable         (forM_)
import DeleteLargest
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "non-duplicate cases" $ forM_ deletes $ \(name, f) ->
    describe name $ do
      it "empty list" $ f [] `shouldBe` []
      it "1..10" $ f [1..10] `shouldBe` [1..9]
      it "10..1" $ f (reverse [1..10]) `shouldBe` (reverse [1..9])
  describe "delete alls" $ forM_ deleteAlls $ \(name, f) ->
    describe name $ do
      it "1, 2, 3, 1, 2, 3" $
        f [1, 2, 3, 1, 2, 3] `shouldBe` [1, 2, 1, 2]
      prop "compare against simple" $ \list ->
        f list `shouldBe` simpleDeleteAll list
  describe "delete firsts" $ forM_ deleteFirsts $ \(name, f) ->
    describe name $ do
      it "1, 2, 3, 1, 2, 3" $
        f [1, 2, 3, 1, 2, 3] `shouldBe` [1, 2, 1, 2, 3]
      prop "compare against simple" $ \list ->
        f list `shouldBe` simpleDeleteFirst list
