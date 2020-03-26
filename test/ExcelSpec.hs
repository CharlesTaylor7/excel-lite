module ExcelSpec where

import Excel
import Test.QuickCheck
import Test.Hspec

test = it

spec = do
  describe "Excel" $ do
    test "literals: readCell gets the literal value set" $ do
      let
        cellId = CellId 0
        value = readCell cellId . setCell cellId (Lit 2) $ emptySheet
      value `shouldBe` pure 2
