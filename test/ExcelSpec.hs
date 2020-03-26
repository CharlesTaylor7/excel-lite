module ExcelSpec where

import Excel
import Test.QuickCheck
import Test.Hspec

test = it

spec = do
  describe "Excel" $ do
    test "literals: readCell gets the literal value set" $ do
      let
        excel = newSheet
        cellId = CellId 0
        value = readCell cellId . setCell cellId (Lit 2) $ excel
      value `shouldBe` pure 2
