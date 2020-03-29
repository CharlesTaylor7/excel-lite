module Excel.OperationsSpec where

import Excel
import Test.QuickCheck
import Test.Hspec

test = it

shouldBeJust :: (Show a, Eq a) => Maybe a -> a -> Expectation
a `shouldBeJust` b = a `shouldBe` Just b

spec = do
  describe "Operations" $ do
    describe "setCell" $ do
      it "updates the max id" $
        let
          id = CellId 4
          sheet = emptySheet
            & setCell id (Lit 5)
        in
          sheet `shouldBe` Sheet [(CellId 4, Lit 5)] id
      it "updates the max id after multiple sets" $
        let
          id1 = CellId 0
          id2 = CellId 1
          id3 = CellId 2
          sheet = emptySheet
            & setCell id1 (Lit 2)
            & setCell id2 (Lit 5)
            & setCell id3 (Exponent (Ref id1) (Ref id2))
        in
          sheet ^. sheet_maxId `shouldBe` CellId 2

    describe "readCell" $ do
      test "reports Nothing when cell is not initialized" $ do
        readCell (CellId 0) emptySheet `shouldBe` Nothing

      describe "literals" $ do
        test "gets the literal value set" $ do
          let
            id = CellId 0
            sheet = emptySheet & setCell id (Lit 2)
            value = readCell id sheet
          value `shouldBeJust` pure 2
      describe "refs" $ do
        test "reads ref when set" $ do
          let
            id1 = CellId 0
            id2 = CellId 2
            sheet = emptySheet
              & setCell id1 (Ref . CellId $ 2)
              & setCell id2 (Lit 5)
          readCell id1 sheet `shouldBeJust` pure 5

        test "reports no ref when ref doesn't exist " $ do
          let
            id = CellId 0
            sheet = emptySheet & setCell id (Ref . CellId $ 2)
            value = readCell id sheet
          value `shouldBeJust` Left InvalidRef

        test "reports cyclic ref when ref refers to itself " $ do
          let
            id = CellId 0
            sheet = emptySheet & setCell id (Ref id)
            value = readCell id sheet
          value `shouldBeJust` Left CyclicReference

        test "reports cyclic ref when refs refer to each other " $
          let
            id1 = CellId 0
            id2 = CellId 7
            sheet = emptySheet
              & setCell id1 (Ref id2)
              & setCell id2 (Ref id1)
            expectedSheet = emptySheet
              & sheet_cells . at id1 ?~ Left CyclicReference
              & sheet_cells . at id2 ?~ Left CyclicReference
              & sheet_maxId .~ id2
          in
            evalSheet sheet `shouldBe` expectedSheet

      test "addition" $
        let
          id1 = CellId 0
          id2 = CellId 1
          id3 = CellId 2
          sheet = emptySheet
            & setCell id1 (Lit 3)
            & setCell id2 (Lit 5)
            & setCell id3 (Add (Ref id1) (Ref id2))
          expectedSheet = emptySheet
            & sheet_cells . at id1 ?~ pure 3
            & sheet_cells . at id2 ?~ pure 5
            & sheet_cells . at id3 ?~ pure 8
            & sheet_maxId .~ id3
        in
          evalSheet sheet `shouldBe` expectedSheet

      test "multiplication" $
        let
          id1 = CellId 0
          id2 = CellId 1
          id3 = CellId 2
          sheet = emptySheet
            & setCell id1 (Lit 3)
            & setCell id2 (Lit 5)
            & setCell id3 (Multiply (Ref id1) (Ref id2))
          expectedSheet = emptySheet
            & sheet_cells . at id1 ?~ pure 3
            & sheet_cells . at id2 ?~ pure 5
            & sheet_cells . at id3 ?~ pure 15
            & sheet_maxId .~ id3
        in
          evalSheet sheet `shouldBe` expectedSheet

      describe "division" $ do
        test "evaluation" $
          let
            id1 = CellId 0
            id2 = CellId 1
            id3 = CellId 2
            sheet = emptySheet
              & setCell id1 (Lit 44)
              & setCell id2 (Lit 5)
              & setCell id3 (Divide (Ref id1) (Ref id2))
            expectedSheet = emptySheet
              & sheet_cells . at id1 ?~ pure 44
              & sheet_cells . at id2 ?~ pure 5
              & sheet_cells . at id3 ?~ pure 8
              & sheet_maxId .~ id3
          in
            evalSheet sheet `shouldBe` expectedSheet
        test "divide by zero error" $
          let
            id1 = CellId 0
            id2 = CellId 1
            id3 = CellId 2
            sheet = emptySheet
              & setCell id1 (Lit 3)
              & setCell id2 (Lit 0)
              & setCell id3 (Divide (Ref id1) (Ref id2))
            expectedSheet = emptySheet
              & sheet_cells . at id1 ?~ pure 3
              & sheet_cells . at id2 ?~ pure 0
              & sheet_cells . at id3 ?~ throwError DivideByZero
              & sheet_maxId .~ id3
          in
            evalSheet sheet `shouldBe` expectedSheet

      describe "exponentiation" $ do
        test "evaluation" $
          let
            id1 = CellId 0
            id2 = CellId 1
            id3 = CellId 2
            sheet = emptySheet
              & setCell id1 (Lit 2)
              & setCell id2 (Lit 5)
              & setCell id3 (Exponent (Ref id1) (Ref id2))
            expectedSheet = emptySheet
              & sheet_cells . at id1 ?~ pure 2
              & sheet_cells . at id2 ?~ pure 5
              & sheet_cells . at id3 ?~ pure 32
              & sheet_maxId .~ id3
          in do
            evalSheet sheet `shouldBe` expectedSheet
        test "negative exponent error" $
          let
            id1 = CellId 0
            id2 = CellId 1
            id3 = CellId 2
            sheet = emptySheet
              & setCell id1 (Lit 2)
              & setCell id2 (Lit (-5))
              & setCell id3 (Exponent (Ref id1) (Ref id2))
            expectedSheet = emptySheet
              & sheet_cells . at id1 ?~ pure 2
              & sheet_cells . at id2 ?~ pure (-5)
              & sheet_cells . at id3 ?~ throwError NegativeExponent
              & sheet_maxId .~ id3
          in
            evalSheet sheet `shouldBe` expectedSheet
