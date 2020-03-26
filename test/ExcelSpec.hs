module ExcelSpec where

import Excel
import Test.QuickCheck
import Test.Hspec

test = it

spec = do
  describe "Excel" $ do
    test "literals: readCell gets the literal value set" $ do
      let
        id = CellId 0
        sheet = emptySheet & setCell id (Lit 2)
        value = readCell id sheet
      value `shouldBe` pure 2

    test "refs: readCell reports no ref when ref doesnt exist " $ do
      let
        id = CellId 0
        sheet = emptySheet & setCell id (Ref . CellId $ 2)
        value = readCell id sheet
      value `shouldBe` Left NonexistentRef

    test "refs: readCell reads ref when set" $ do
      let
        id1 = CellId 0
        id2 = CellId 2
        sheet = emptySheet
          & setCell id1 (Ref . CellId $ 2)
          & setCell id2 (Lit 5)
        value = readCell id1 sheet

      value `shouldBe` pure 5
