module Excel.ParserSpec where

import Excel
import Test.QuickCheck
import Test.Hspec

spec = do
  describe "Parser" $ do
    describe "simple assignments" $ do
      it "handles literal assignments" $
        let
          parsed = parseInput "$1 = 3"
          expected = _Right . _Assign #
            Assignment (CellId 1) (Lit 3)
        in
          parsed `shouldBe` expected
      it "handles cell refs & nesting" $
        let
          parsed = parseInput "$1 = $2 * $3 + 43"
          cell = Ref . CellId
          expected = _Right . _Assign #
            Assignment (CellId 1) (Add (Multiply (cell 2) (cell 3)) (Lit 43))
        in
          parsed `shouldBe` expected
      it "handles subtraction & division" $
        let
          parsed = parseInput "$1 = ($2 - $3) / 43"
          cell = Ref . CellId
          expected = _Right . _Assign #
            Assignment (CellId 1) (Divide (Subtract (cell 2) (cell 3)) (Lit 43))
        in
          parsed `shouldBe` expected

      it "handles exponents" $
        let
          parsed = parseInput "$1 = 2^0"
          cell = Ref . CellId
          expected = _Right . _Assign #
            Assignment (CellId 1) (Exponent (Lit 2) (Lit 0))
        in
          parsed `shouldBe` expected

      it "handles when there is no space between a cell ref & an operator" $
        let
          parsed = parseInput "$1 =2-$4"
          cell = Ref . CellId
          expected = _Right . _Assign #
            Assignment (CellId 1) (Subtract (Lit 2) (cell 4))
        in
          parsed `shouldBe` expected
