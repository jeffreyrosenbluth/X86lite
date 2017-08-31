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
  describe "dummy test" $
    it "does nothing" $ do
      0 `shouldBe` 0
  describe "mapAddr tests" $
    it "mapAddr1" $ do
      mapAddr 0x40FFF8 `shouldBe` Just 65528
