module Overflow where

import           Data.Int (Int64)

data T = T
  { value    :: Int64
  , overflow :: Bool
  }

ok :: Int64 -> T
ok i = T i False

withOverflow1 :: (Integer -> Integer) -> (Int64 -> Int64) -> Int64 -> T
withOverflow1 g f i = T v o
  where
    v = f i
    bigV = fromIntegral v :: Integer
    bigI = fromIntegral i :: Integer
    o = bigV /= (g bigI)

withOverflow2 :: (Integer -> Integer -> Integer)
              -> (Int64 -> Int64 -> Int64)
              -> Int64 -> Int64 -> T
withOverflow2 g f i j = T v o
  where
    v = f i j
    bigV = fromIntegral v :: Integer
    bigI = fromIntegral i :: Integer
    bigJ = fromIntegral j :: Integer
    o = bigV /= (g bigI bigJ)

neg64, succ64, pred64 :: Int64 -> T
neg64 = withOverflow1 negate negate
succ64 = withOverflow1 succ succ
pred64 = withOverflow1 pred pred

add64, sub64, mul64 :: Int64 -> Int64 -> T
add64 = withOverflow2 (+) (+)
sub64 = withOverflow2 (-) (-)
mul64 = withOverflow2 (*) (*)
