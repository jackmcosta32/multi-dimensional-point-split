module ListsSpec where

import SpecHelper
import Lists (popIndexes, findIndexes)

spec :: Spec
spec = do
  describe "[Function] popIndexes" $ do
    context "with elements = [1, 2]" $ do
      let elements = [1, 2]

      it "should return [1] for indexes [1]" $
        popIndexes [1] elements `shouldBe` [1]

      it "should return [2] for indexes [0]" $
        popIndexes [0] elements `shouldBe` [2]

    context "with elements = [1, 2, 4, 7, 6]" $ do
      let elements = [1, 2, 4, 7, 6]

      it "should return [1, 4, 6] for indexes [1, 3]" $
        popIndexes [1, 3] elements `shouldBe` [1, 4, 6]

      it "should return [7] for indexes [0, 1, 2, 4]" $
        popIndexes [0, 1, 2, 4] elements `shouldBe` [7]

      it "should return [7] for indexes [4, 1, 2, 0]" $
        popIndexes [4, 1, 2, 0] elements `shouldBe` [7]

  describe "[Function] findIndexes" $ do
    context "with elements = [1, 2]" $ do
      let elements = [1, 2]

      it "should return [0] for match 1" $
        findIndexes 1 elements `shouldBe` [0]

      it "should return [1] for match 2" $
        findIndexes 2 elements `shouldBe` [1]

    context "with elements = [1, 1, 1, 1, 1]" $ do
      let elements = [1, 1, 1, 1, 1]

      it "should return [0, 1, 2, 3, 4] for match 1" $
        findIndexes 1 elements `shouldBe` [0, 1, 2, 3, 4]

      it "should return [] for match 2" $
        findIndexes 2 elements `shouldBe` []

    context "with elements = [1, 2, 4, 5, 1, 7, 1, 2]" $ do
      let elements = [1, 2, 4, 5, 1, 7, 1, 2]

      it "should return [0, 4, 6] for match 1" $
        findIndexes 1 elements `shouldBe` [0, 4, 6]

      it "should return [1, 7] for match 2" $
        findIndexes 2 elements `shouldBe` [1, 7]

      it "should return [2] for match 4" $
        findIndexes 4 elements `shouldBe` [2]

main :: IO ()
main = hspec spec
    