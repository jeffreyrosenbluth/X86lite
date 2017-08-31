{-# LANGUAGE LambdaCase #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  X86
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
-- X86lite simulator
-------------------------------------------------------------------------------
module Simulator where

import           X86

import           Data.Bits                   (shiftL, shiftR, (.&.), (.|.))
import           Data.Char                   (ord)
import           Data.Int                    (Int64)
import           Data.STRef                  (STRef)
import           Data.Text.Lazy              (unpack)
import           Data.Vector.Unboxed.Mutable (STVector)
import           Data.Word                   (Word8)

-- Simulator machine state ----------------------------------------------------
memBot, memTop, insSize, exitAddr :: Int64
memBot   = 0x400000 -- lowest valid address
memTop   = 0x410000 -- one past the last byte in memory
insSize  = 4        -- assume we have a 4-byte encoding
exitAddr = 0xfdead  -- halt when m.regs(%rip) = exitAddr

memSize :: Int
memSize = fromIntegral (memTop - memBot)

-- | Including Rip
nregs :: Int
nregs = 17

{- Your simulator should raise this exception if it tries to read from or
   store to an address not within the valid address space.

XXX exception X86lite_segfault

   The simulator memory maps addresses to symbolic bytes.  Symbolic
   bytes are either actual data indicated by the Byte constructor or
   'symbolic instructions' that take up four bytes for the purposes of
   layout.

   The symbolic bytes abstract away from the details of how
   instructions are represented in memory.  Each instruction takes
   exactly four consecutive bytes, where the first byte InsB0 stores
   the actual instruction, and the next three bytes are InsFrag
   elements, which aren't valid data.

   For example, the two-instruction sequence:
        at&t syntax             ocaml syntax
      movq %rdi, (%rsp)       Movq,  [~%Rdi; Ind2 Rsp]
      decq %rdi               Decq,  [~%Rdi]

   is represented by the following elements of the mem array (starting
   at address 0x400000):

       0x400000 :  InsB0 (Movq,  [~%Rdi; Ind2 Rsp])
       0x400001 :  InsFrag
       0x400002 :  InsFrag
       0x400003 :  InsFrag
       0x400004 :  InsB0 (Decq,  [~%Rdi])
       0x400005 :  InsFrag
       0x400006 :  InsFrag
       0x400007 :  InsFrag
-}

data Sbyte
  = InsB0 Ins   -- ^ 1st byte of an instruction
  | InsFrag     -- ^ 2nd, 3rd or 4th byte of an instruction
  | Byte Word8  -- ^ non-instruction byte

-- | Memory maps addresses to symbolic bytes
type Mem = STVector Int64

-- | Flags for condition codes
data Flags s = Flags
  { fo :: STRef s Bool
  , fs :: STRef s Bool
  , fz :: STRef s Bool
  }

-- | Register Files
type Regs = STVector Int64

-- | Complete machine state
data Mach s = Mach
  { flags :: Flags s
  , regs  :: Regs s
  , mem   :: Mem s
  }

-- Simulator helper functions -------------------------------------------------

-- | The index of a register in the regs vector
rind :: Reg -> Int
rind = \case
  Rip -> 16
  Rax -> 0
  Rbx -> 1
  Rcx -> 2
  Rdx -> 3
  Rsi -> 4
  Rdi -> 5
  Rbp -> 6
  Rsp -> 7
  R08 -> 8
  R09 -> 9
  R10 -> 10
  R11 -> 11
  R12 -> 12
  R13 -> 13
  R14 -> 14
  R15 -> 15

-- Helper functions for reading/writing sbyts ---------------------------------

-- | Convert a Char to an sbyte
byte :: Char -> Sbyte
byte = Byte . fromIntegral . ord

-- | Convert Int64 to its sbyte representation
sbytesOfInt64 :: Int64 -> [Sbyte]
sbytesOfInt64 i = f <$> [0, 8, 16, 24, 32, 40, 48, 56]
  where
    f = Byte . fromIntegral . (.&. 0xff) . shiftR i

-- | Convert an Sbyte representation to an Int64
int64OfSbytes :: [Sbyte] -> Int64
int64OfSbytes = foldr f 0
  where
    f b i = case b of
      Byte c -> shiftL i 8 .|. fromIntegral c
      _      -> 0

-- | Convert a String to its Sbyte representation.
sbytesOfString :: String -> [Sbyte]
sbytesOfString s = go [Byte 0] (length s -1)
  where
    go acc i
      | i <0 = acc
      | otherwise = go (Byte (fromIntegral . ord $ s !! i) : acc) (pred i)

-- | Serialize an instruction to Sbytes.
sbytesOfIns :: Ins -> [Sbyte]
sbytesOfIns ins@(op, args) =
  if all check  args
    then [InsB0 ins, InsFrag, InsFrag, InsFrag]
    else error "sbytesOfIns tried to serialize a label!"
  where
    check (Imm (Lbl _))     = False
    check (Ind1 (Lbl _))    = False
    check (Ind3 (Lbl _, _)) = False
    check _                 = True

-- | Serialize a data element to Sbytes.
sbytesOfData :: Data -> [Sbyte]
sbytesOfData = \case
  Quad (Lit i) -> sbytesOfInt64 i
  Asciz s -> sbytesOfString $ unpack s
  Quad (Lbl _) -> error "sybytesOfData tried to serialize a label!"

-- | Interpret a condition code with respect ot the given flags.
interpCnd :: Cnd -> Bool
interpCnd c = error "interpCnd not implemented"

-- | Maps an X86Liet address into Just haskell list index,
--   or Nothing if the address is not within the legal address space.
mapAddr :: Quad -> Maybe Int
mapAddr q = error "mapAddr not implemented"

-- | Simulates one step of the machine:
--    * fetch the instruction at %rip
--    * compute the source and/or destination information from the operands
--    * simulate the instruction semantics
--    * update the registers and/or memory appropriately
--    * set the condition flags
step :: Mach s -> ()
step m = error "step not implemented"

-- | Runs the maching until the rip  register reaches a designated memory
--   address.
run :: Mach s -> Int64
run m = undefined

-- assemblin and linking ------------------------------------------------------

-- | A representation of the executable
data Exec = Exec
  { entry   :: Quad    -- ^ address of the entry point
  , textPos :: Quad    -- ^ starting address of the code
  , dataPos :: Quad    -- ^ starting address of the date
  , textSeg :: [Sbyte] -- ^ contents of the text segment
  , dataSeg :: [Sbyte] -- ^ contents of the data segment
  }

{-  Convert an X86 program into an object file:
   - separate the text and data segments
   - compute the size of each segment
      Note: the size of an Asciz string section is (1 + the string length)

   - resolve the labels to concrete addresses and 'patch' the instructions to
     replace Lbl values with the corresponding Imm values.

   - the text segment starts at the lowest address
   - the data segment starts after the text segment

  HINT: List.fold_left and List.fold_right are your friends.
 -}

assemble :: Prog -> Exec
assemble = undefined

{- Convert an object file into an executable machine state.
    - allocate the mem array
    - set up the memory state by writing the symbolic bytes to the
      appropriate locations
    - create the inital register state
      - initialize rip to the entry point address
      - initializes rsp to the last word in memory
      - the other registers are initialized to 0
    - the condition code flags start as 'false'

  Hint: The Array.make, Array.blit, and Array.of_list library functions
  may be of use.
-}

load :: Exec -> Mach s
load = undefined
