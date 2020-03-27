module ExcelSpec where

import Excel
import Test.QuickCheck
import Test.Hspec

test = it

spec = do
  describe "Excel" $ do
    describe "readCell" $ do
      test "reports EmptyCell when cell is not initialized" $ do
        readCell (CellId 0) emptySheet `shouldBe` Left EmptyCell

      describe "literals" $ do
        test "gets the literal value set" $ do
          let
            id = CellId 0
            sheet = emptySheet & setCell id (Lit 2)
            value = readCell id sheet
          value `shouldBe` pure 2
      describe "refs" $ do
        test "reads ref when set" $ do
          let
            id1 = CellId 0
            id2 = CellId 2
            sheet = emptySheet
              & setCell id1 (Ref . CellId $ 2)
              & setCell id2 (Lit 5)
          readCell id1 sheet `shouldBe` pure 5

        test "reports no ref when ref doesn't exist " $ do
          let
            id = CellId 0
            sheet = emptySheet & setCell id (Ref . CellId $ 2)
            value = readCell id sheet
          value `shouldBe` Left InvalidRef

        test "reports cyclic ref when ref refers to itself " $ do
          let
            id = CellId 0
            sheet = emptySheet & setCell id (Ref id)
            value = readCell id sheet
          value `shouldBe` Left CyclicReference

        test "reports cyclic ref when refs refer to each other " $ do
          let
            id1 = CellId 0
            id2 = CellId 0
            sheet = emptySheet
              & setCell id1 (Ref id2)
              & setCell id2 (Ref id1)
            expectedSheet = emptySheet
              & _Sheet . at id1 ?~ Left CyclicReference
              & _Sheet . at id2 ?~ Left CyclicReference

          evalSheet sheet `shouldBe` expectedSheet
