{-# LANGUAGE LambdaCase #-}
-------------------------------------------------------------------------------
-- |
-- Module      :  X86
-- Copyright   :  (c) 2015 Jeffrey Rosenbluth
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  jeffrey.rosenbluth@gmail.com
-- X86lite language representation
-------------------------------------------------------------------------------

module X86 where

import           Data.Int       (Int64)
import           Data.Monoid
import           Data.Text.Lazy (Text, pack, unpack)
import qualified Data.Text.Lazy as T

-- Assembler syntax

-- | Labels for code blocks and global data.
type Lbl = Text

type Quad = Int64

-- | Immediate operands
data Imm
  = Lit Quad
  | Lbl Lbl

-- | Registers:
--     instruction pointers: rip
--     arguments: rdi, rsi, rdx, rcx, r09, r08
--     callee-save: rbx, rbp, r12-r15
data Reg
  = Rip
  | Rax | Rbx | Rcx | Rdx | Rsi | Rdi | Rbp | Rsp
  | R08 | R09 | R10 | R11 | R12 | R13 | R14 | R15

data Operand
  = Imm Imm         -- ^ immediate
  | Reg Reg         -- ^ register
  | Ind1 Imm        -- ^ indirect: displacement
  | Ind2 Reg        -- ^ indirect: (%reg)
  | Ind3 (Imm, Reg) -- ^ indirect: displacement(%reg)

-- | Condition Codes
data Cnd = Eq | Neq | Gt | Ge | Lt | Le

data Opcode
  = Movq  | Pushq | Popq
  | Leaq
  | Incq  | Decq  | Negq  | Notq
  | Addq  | Subq  | Imulq | Xorq | Orq | Andq
  | Shlq  | Sarq  | Shrq
  | Jmp   | J Cnd
  | Cmpq  | Set Cnd
  | Callq | Retq

-- | An instruction is an opcode plus it operands.
--   Note that arity and other constraints about the operands
--   are not checked

type Ins = (Opcode, [Operand])

data Data
  = Asciz Text
  | Quad Imm

data Asm
  = Text [Ins]  -- ^ code
  | Data [Data] -- ^ data

-- | Labeled blocks of data or code
data Elem = Elem
  { lbl    :: Lbl
  , global :: Bool
  , asm    :: Asm
  }

type Prog = [Elem]

-- | Provide some syntactic sugar for writing x86 code in haskell files,
kInt64 :: Int -> Operand
kInt64 i = Imm $ Lit (fromIntegral i)

kLabel :: Text -> Operand
kLabel l = Imm $ Lbl l

-- | Helper functions for building blocks of data or code
dataElem :: Lbl -> [Data] -> Elem
dataElem l ds = Elem l True (Data ds)

textElem :: Lbl -> [Ins] -> Elem
textElem l is = Elem l False (Text is)

gtextElem :: Lbl -> [Ins] -> Elem
gtextElem l is = Elem l True (Text is)

-- Pretty printing
ppReg :: Reg -> String
ppReg = \case
  Rip -> "%rip"
  Rax -> "%rax"
  Rbx -> "%rbx"
  Rcx -> "%rcx"
  Rdx -> "%rdx"
  Rsi -> "%rsi"
  Rdi -> "%rdi"
  Rbp -> "%rbp"
  Rsp -> "%rsp"
  R08 -> "%r8"
  R09 -> "%r9"
  R10 -> "%r10"
  R11 -> "%r11"
  R12 -> "%r12"
  R13 -> "%r13"
  R14 -> "%r14"
  R15 -> "%r15"

ppByteReg :: Reg -> String
ppByteReg = \case
  Rip -> error "%rip used as byte register"
  Rax -> "%al"
  Rbx -> "%bl"
  Rcx -> "%cl"
  Rdx -> "%dl"
  Rsi -> "%sil"
  Rdi -> "%dil"
  Rbp -> "%bpl"
  Rsp -> "%spl"
  R08 -> "%r8b"
  R09 -> "%r9b"
  R10 -> "%r10b"
  R11 -> "%r11b"
  R12 -> "%r12b"
  R13 -> "%r13b"
  R14 -> "%r14b"
  R15 -> "%r15b"

ppLabel :: Lbl -> String
ppLabel l = unpack l

ppImm :: Imm -> String
ppImm = \case
  Lit i -> show i
  Lbl l -> ppLabel l

ppOperand :: Operand -> String
ppOperand = \case
  Imm i       -> "$" <> ppImm i
  Reg r       -> ppReg r
  Ind1 i      -> ppImm i
  Ind2 r      -> "("  <> ppReg r <> ")"
  Ind3 (i, r) -> ppImm i <> "(" <> ppReg r <> ")"

ppByteOperand :: Operand -> String
ppByteOperand = \case
  Imm i       -> "$" <> ppImm i
  Reg r       -> ppByteReg r
  Ind1 i      -> ppImm i
  Ind2 r      -> "("  <> ppReg r <> ")"
  Ind3 (i, r) -> ppImm i <> "(" <> ppReg r <> ")"

ppJmpOperand :: Operand -> String
ppJmpOperand = \case
  Imm i       -> "$"  <> ppImm i
  Reg r       -> "*"  <> ppReg r
  Ind1 i      -> "*"  <> ppImm i
  Ind2 r      -> "*(" <> ppReg r <> ")"
  Ind3 (i, r) -> "*"  <> ppImm i <> "(" <> ppReg r <> ")"

ppCnd :: Cnd -> String
ppCnd = \case
  Eq  -> "e"
  Neq -> "ne"
  Gt  -> "g"
  Ge  -> "ge"
  Lt  -> "l"
  Le  -> "le"

ppOpcode :: Opcode -> String
ppOpcode = \case
  Movq  -> "movq"
  Pushq -> "pushq"
  Popq  -> "popq"
  Leaq  -> "leaq"
  Incq  -> "incq"
  Decq  -> "decq"
  Negq  -> "negq"
  Notq  -> "notq"
  Addq  -> "addq"
  Subq  -> "subq"
  Imulq -> "imulq"
  Xorq  -> "xorq"
  Orq   -> "orq"
  Andq  -> "andq"
  Shlq  -> "shlq"
  Sarq  -> "sarq"
  Shrq  -> "shrq"
  Jmp   -> "jmp"
  J c   -> "J" <> ppCnd c
  Cmpq  -> "cmpq"
  Set c -> "set" <> ppCnd c
  Callq -> "callq"
  Retq  -> "retq"

mapConcat :: String -> (a -> String) -> [a] -> String
mapConcat s f l = mconcat $ s : (f <$> l)

ppShift :: Opcode -> [Operand] -> String
ppShift op os = case os of
  args@[Imm i, dst] -> "\t" <> ppOpcode op <> "\t" <> mapConcat ", " ppOperand args
  [Reg Rcx, dst]    ->
    let s  = ppOpcode op
        cl = ppOperand dst
    in  "\t" <> s <> "\t" <> cl <> ", " <> s
  args              ->
    let s = mapConcat ", " ppOperand args
    in  error $ "shift instruction has invaled operands" <> s

ppIns :: Ins -> String
ppIns (op, args) = case op of
  Shlq  -> ppShift op args
  Sarq  -> ppShift op args
  Shrq  -> ppShift op args
  J _   -> "\t" <> ppOpcode op <> "\t" <> mapConcat ", " ppJmpOperand args
  Jmp   -> "\t" <> ppOpcode op <> "\t" <> mapConcat ", " ppJmpOperand args
  Callq -> "\t" <> ppOpcode op <> "\t" <> mapConcat ", " ppJmpOperand args
  Set _ -> "\t" <> ppOpcode op <> "\t" <> mapConcat ", " ppByteOperand args
  _     -> "\t" <> ppOpcode op <> "\t" <> mapConcat ", " ppOperand args

ppData :: Data -> String
ppData = \case
  Asciz s -> "\t.asciz\t" <> "\"" <> unpack s <> "\""
  Quad i  -> "\t.quad\t"  <> ppImm i

ppAsm :: Asm -> String
ppAsm = \case
  Text is -> "\t.text\n" <> mapConcat "\n" ppIns is
  Data ds -> "\t.data\n" <> mapConcat "\n" ppData ds

ppElem :: Elem -> String
ppElem (Elem l g a) =
  let (sec, body) = case a of
        Text is -> ("\t.text\n", mapConcat "\n" ppIns is)
        Data ds -> ("\t.data\n", mapConcat "\n" ppData ds)
      glb = if g then "\t.globl\t" <> ppLabel l <> "\n" else ""
  in  sec <> glb <> ppLabel l <> ":\n" <> body

ppProg :: Prog -> String
ppProg p = mconcat $ "\n" : (ppElem <$> p)
