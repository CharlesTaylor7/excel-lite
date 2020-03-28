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
          expected = Just $ Write (CellId 1) (Lit 3)
        in
          parsed `shouldBe` expected
