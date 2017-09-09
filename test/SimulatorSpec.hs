module SimulatorSpec (spec) where

import           Simulator
import           Test.Hspec
import           X86


helloworldDataseg :: [Sbyte]
helloworldDataseg =
  [ byte 'c', Byte 0, Byte 0, Byte 0
  , Byte 0, Byte 0, Byte 0, Byte 0
  , byte 'H', byte 'e' , byte 'l', byte 'l'
  , byte 'o', byte ',', byte ' ', byte 'w'
  , byte 'o', byte 'r', byte 'l', byte 'd'
  , byte '!', Byte 0 ]

helloworldTextseg :: [Sbyte]
helloworldTextseg =
  [ InsB0 (Xorq, [Reg Rax, Reg Rax]), InsFrag, InsFrag, InsFrag
  , InsB0 (Movq, [Imm (Lit 100), Reg Rax]), InsFrag, InsFrag, InsFrag
  , InsB0 (Retq, []), InsFrag, InsFrag, InsFrag
  , InsB0 (Xorq, [Reg Rax, Reg Rax]), InsFrag, InsFrag, InsFrag
  , InsB0 (Movq, [Ind1 (Lit 0x400018), Reg Rax]), InsFrag, InsFrag, InsFrag
  , InsB0 (Retq, []), InsFrag, InsFrag, InsFrag
  ]

spec :: Spec
spec = do
  describe "mapAddr tests" $
    it "Address" $ do
      mapAddr 0x40FFF8 `shouldBe` Just 65528
      mapAddr 0x4000FF `shouldBe` Just 255
      mapAddr 0x400000 `shouldBe` Just 0
      mapAddr 0x0000000000000000 `shouldBe` Nothing
      mapAddr 0xFFFFFFFFFFFFFFFD `shouldBe` Nothing
  describe "interCnd tests" $
    it "Flags" $ let t = [True, True, True] in do
      interpCnd False False False <$> [Neq, Gt, Ge] `shouldBe` t
      interpCnd False False True  <$> [Eq, Le, Ge]  `shouldBe` t
      interpCnd False True False  <$> [Neq, Le, Lt] `shouldBe` t
      interpCnd False True True   <$> [Eq, Le, Lt]  `shouldBe` t
      interpCnd True False False  <$> [Neq, Le, Lt] `shouldBe` t
      interpCnd True False True   <$> [Eq, Le, Lt]  `shouldBe` t
      interpCnd True True False   <$> [Neq, Gt, Ge] `shouldBe` t
      interpCnd True True True    <$> [Eq, Le, Ge]  `shouldBe` t
