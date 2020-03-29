module Excel.PrettySpec where

import Excel
import Test.QuickCheck
import Test.Hspec

test = it

spec = do
  describe "Pretty" $ do
    test "pretty Sheet: labels cells, shows expressions & values" $
        let
          id1 = CellId 0
          id2 = CellId 1
          id3 = CellId 2
          sheet = emptySheet
            & setCell id1 (Lit 2)
            & setCell id2 (Lit 5)
            & setCell id3 (Exponent (Ref id1) (Ref id2))
          expected = ""
        in
          pretty sheet `shouldBe` expected
