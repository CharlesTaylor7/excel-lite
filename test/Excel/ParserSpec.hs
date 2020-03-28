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
          expected = pure $ Write (CellId 1) (Lit 3)
        in
          parsed `shouldBe` expected
      it "handles cell refs & nesting" $
        let
          parsed = parseInput "$1 = $2 * $3 + 43"
          cell = Ref . CellId
          expected = pure $ Write (CellId 1) (Add (Multiply (cell 2) (cell 3)) (Lit 43))
        in
          parsed `shouldBe` expected
      it "handles subtraction & division" $
        let
          parsed = parseInput "$1 = ($2 - $3) / 43"
          cell = Ref . CellId
          expected = pure $ Write (CellId 1) (Divide (Subtract (cell 2) (cell 3)) (Lit 43))
        in
          parsed `shouldBe` expected
