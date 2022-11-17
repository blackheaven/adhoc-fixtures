module Data.Fixtures.AdhocSpec (main, spec) where

import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Adhoc fixtures" $
    return ()
