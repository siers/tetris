module TetrisSpec where

import Test.Hspec
import Test.QuickCheck
import Tetris

instance Arbitrary BlockRotation where
  arbitrary = toEnum <$> chooseInt (0, 3)

spec :: Spec
spec = do
  describe "rotations" $ do
    let b = initBlock L
    let r = rotateBlockRaw

    it "should rotate blocks" $ do
      property $ \r1 ->
        property $ \r2 ->
          (r r1 . r r2) b `shouldBe` r (r1 <> r2) b
