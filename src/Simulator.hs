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

import           Data.Int                    (Int64)
import           Data.STRef                  (STRef)
import           Data.Vector.Unboxed.Mutable
import           Data.Word                   (Word8)

-- Simulator machine state
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

-- Simulator helper functions

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
