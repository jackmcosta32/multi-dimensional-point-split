module GroupsSpec where

import SpecHelper
import Groups (arrangeConnections, getBreakpoints)

spec :: Spec
spec = do
  describe "[Function] getBreakpoints" $ do
    context "with weights = [4, 5, 6]" $ do
      let weights = [4, 5, 6]

      it "should return [3, 2, 1] for amount = 3" $
        getBreakpoints 3 weights `shouldBe` [3, 2, 1]

      it "should return [3, 2] for amount = 2" $
        getBreakpoints 2 weights `shouldBe` [3, 2]

      it "should return [3] for amount = 1" $
        getBreakpoints 1 weights `shouldBe` [3]

    context "with weights = [1, 10, 5, 57]" $ do
      let weights = [1, 10, 5, 57]

      it "should return [4, 2, 3, 1] for amount = 4" $
        getBreakpoints 4 weights `shouldBe` [4, 2, 3, 1]

      it "should return [4, 2, 3] for amount = 2" $
        getBreakpoints 3 weights `shouldBe` [4, 2, 3]

      it "should return [4, 2] for amount = 2" $
        getBreakpoints 2 weights `shouldBe` [4, 2]

      it "should return [4] for amount = 1" $
        getBreakpoints 1 weights `shouldBe` [4]

  describe "[Function] arrangeConnections" $ do
    context "with connections = [[1, 1], [2, 1]]" $ do
      let connections = [[1, 1], [2, 1]]

      it "should be able to arrange a connection for k = 1" $
        arrangeConnections 1 connections `shouldBe` [[[1, 1], [2, 1]]]

      it "should be able to arrange a connection for k = 2" $
        arrangeConnections 2 connections `shouldBe` [[[1, 1]], [[2, 1]]]

    context "with connections = [[1, 1], [2, 1], [5, 5]]" $ do
      let connections = [[1, 1], [2, 1], [5, 5]]

      it "should be able to arrange a connection for k = 1" $
        arrangeConnections 1 connections `shouldBe` [[[1, 1], [2, 1], [5, 5]]]

      it "should be able to arrange a connection for k = 2" $
        arrangeConnections 2 connections `shouldBe` [[[1, 1], [2, 1]], [[5, 5]]]

      it "should be able to arrange a connection for k = 3" $
        arrangeConnections 3 connections `shouldBe` [[[1, 1]], [[2, 1]], [[5, 5]]]

    context "with connections = [[1, 1], [2, 1], [5, 5], [5, 6]]" $ do
      let connections = [[1, 1], [2, 1], [5, 5], [5, 6]]

      it "should be able to arrange a connection for k = 1" $
        arrangeConnections 1 connections `shouldBe` [[[1, 1], [2, 1], [5, 5], [5, 6]]]

      it "should be able to arrange a connection for k = 2" $
        arrangeConnections 2 connections `shouldBe` [[[1, 1], [2, 1]], [[5, 5], [5, 6]]]

      it "should be able to arrange a connection for k = 3" $
        arrangeConnections 3 connections `shouldBe` [[[1, 1]], [[2, 1]], [[5, 5], [5, 6]]]
 
      it "should be able to arrange a connection for k = 4" $
        arrangeConnections 4 connections `shouldBe` [[[1, 1]], [[2, 1]], [[5, 5]], [[5, 6]]]

    context "with connections = [[1, 1], [2, 1], [5, 5], [5, 6], [25, 9]]" $ do
      let connections = [[1, 1], [2, 1], [5, 5], [5, 6], [25, 9]]

      it "should be able to arrange a connection for k = 1" $
        arrangeConnections 1 connections `shouldBe` [[[1, 1], [2, 1], [5, 5], [5, 6], [25, 9]]]

      it "should be able to arrange a connection for k = 2" $
        arrangeConnections 2 connections `shouldBe` [[[1, 1], [2, 1], [5, 5], [5, 6]], [[25, 9]]]

      it "should be able to arrange a connection for k = 3" $
        arrangeConnections 3 connections `shouldBe` [[[1, 1], [2, 1]], [[5, 5], [5, 6]], [[25, 9]]]
 
      it "should be able to arrange a connection for k = 4" $
        arrangeConnections 4 connections `shouldBe` [[[1, 1]], [[2, 1]], [[5, 5], [5, 6]], [[25, 9]]]
 
      it "should be able to arrange a connection for k = 5" $
        arrangeConnections 5 connections `shouldBe` [[[1, 1]], [[2, 1]], [[5, 5]], [[5, 6]], [[25, 9]]]

    context "with connections = [[1, 1], [2, 1], [5, 5], [5, 6], [25, 9], [26, 9]]" $ do
      let connections = [[1, 1], [2, 1], [5, 5], [5, 6], [25, 9], [26, 9]]

      it "should be able to arrange a connection for k = 1" $
        arrangeConnections 1 connections `shouldBe` [[[1, 1], [2, 1], [5, 5], [5, 6], [25, 9], [26, 9]]]

      it "should be able to arrange a connection for k = 2" $
        arrangeConnections 2 connections `shouldBe` [[[1, 1], [2, 1], [5, 5], [5, 6]], [[25, 9], [26, 9]]]

      it "should be able to arrange a connection for k = 3" $
        arrangeConnections 3 connections `shouldBe` [[[1, 1], [2, 1]], [[5, 5], [5, 6]], [[25, 9], [26, 9]]]
 
      it "should be able to arrange a connection for k = 4" $
        arrangeConnections 4 connections `shouldBe` [[[1, 1]], [[2, 1]], [[5, 5], [5, 6]], [[25, 9], [26, 9]]]
 
      it "should be able to arrange a connection for k = 5" $
        arrangeConnections 5 connections `shouldBe` [[[1, 1]], [[2, 1]], [[5, 5]], [[5, 6]], [[25, 9], [26, 9]]]
 
      it "should be able to arrange a connection for k = 6" $
        arrangeConnections 6 connections `shouldBe` [[[1, 1]], [[2, 1]], [[5, 5]], [[5, 6]], [[25, 9]], [[26, 9]]]

main :: IO ()
main = hspec spec
    