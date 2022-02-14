module ConnectionsSpec where

import SpecHelper
import Connections (connectPoints)

spec :: Spec
spec = 
  describe "[Function] connectPoints" $ do
    context "with points = [[1, 1], [5, 6], [2, 1], [33, 12]]" $ do
      let points = [[1, 1], [5, 6], [2, 1], [33, 12]]

      it "should return a list where the first point matches the first point of the input list." $ do
        let result = connectPoints points
        head result `shouldBe` [1, 1]

      it "should order the points on an ascending order, returning [[1, 1], [2, 1], [5, 6], [33, 12]]" $
        connectPoints points `shouldBe`[[1, 1], [2, 1], [5, 6], [33, 12]]

    context "with points = [[1, 1], [0, 0], [2, 2]]" $ do
      let points = [[1, 1], [0, 0], [2, 2]]

      it "should return a list where the first point matches the first point of the input list." $ do
        let result = connectPoints points
        head result `shouldBe` [1, 1]

      it "should consider the element position in the list when choosing the nearest point" $
        connectPoints points `shouldBe`[[1, 1], [0, 0], [2, 2]]

main :: IO ()
main = hspec spec
