

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Main

main :: IO ()
main = do
    hspec $ describe "tmp_rewrite.Main.justOne" $ do
        it "returns the Just value if only one of the passed functions results in a Just value" $ do
            justOne [(\f x -> if(x == 1) then Just 1 else Nothing), (\f _ -> Nothing)] 1 `shouldBe` (Just 1) $ do
                justOne [(\f _ -> Nothing)] `shouldBe` Nothing