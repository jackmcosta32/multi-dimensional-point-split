module ConnectionsSpec where

import SpecHelper
import Connections (connectPoints)

spec :: Spec
spec = 
  describe "[Module] Connections" $
    context "with function connectPoints" $ do
      it "should sort a list of points by ascending by their euclidean distance" $ do
        let points = [[1.0,1.0], [5.0,6.0], [2.0,1.0], [33.0,12.0], [5.0,5.0], [26.0,9.0], [33.0,11.0], [25.0,9.0]]
        putStr "Inital group of points"
        print points

        let connections = connectPoints points
        putStr "Ordered group of points"
        print connections

        connections `shouldBe` [[1.0,1.0],[2.0,1.0],[5.0,5.0],[5.0,6.0],[25.0,9.0],[26.0,9.0],[33.0,11.0],[33.0,12.0]]

      it "should match the initial point of the given points list and the first point of the function return" $ do
        let points = [[5.0,6.0], [2.0,1.0], [26.0,9.0], [33.0,11.0], [25.0,9.0]]
        let connections = connectPoints points

        head connections `shouldBe` head points

main :: IO ()
main = hspec spec
    