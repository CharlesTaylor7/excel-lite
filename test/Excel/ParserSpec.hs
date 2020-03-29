module Excel.ParserSpec where

import Excel
import Test.QuickCheck
import Test.Hspec

spec = do
  describe "Parser" $ do
    describe "expression parser" $ do
      it "handles literal assignments" $
        let
          parsed = parseExpr "3"
          expected = _Right #
            Lit 3
        in
          parsed `shouldBe` expected
      it "handles cell refs & nesting" $
        let
          parsed = parseExpr "$2 * $3 + 43"
          cell = Ref . CellId
          expected = _Right #
            Add (Multiply (cell 2) (cell 3)) (Lit 43)
        in
          parsed `shouldBe` expected
      it "handles subtraction & division" $
        let
          parsed = parseExpr "($2 - $3) / 43"
          cell = Ref . CellId
          expected = _Right #
            Divide (Subtract (cell 2) (cell 3)) (Lit 43)
        in
          parsed `shouldBe` expected

      it "handles exponents" $
        let
          parsed = parseExpr "2^0"
          cell = Ref . CellId
          expected = _Right #
            Exponent (Lit 2) (Lit 0)
        in
          parsed `shouldBe` expected

      it "handles when there is no space between a cell ref & an operator" $
        let
          parsed = parseExpr "2-$4"
          cell = Ref . CellId
          expected = _Right #
            Subtract (Lit 2) (cell 4)
        in
          parsed `shouldBe` expected
