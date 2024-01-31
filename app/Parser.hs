module Parser (IR (..), runParse) where

import Data.Word (Word8)
import Data.Void (Void)
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char (char)

type Parser a = Parsec Void Text a

data Instruction
  = Inc
  | Dec
  | StepR
  | StepL
  | In
  | Out
  | Loop [Instruction]
  | Noop
  deriving (Show, Eq)

data IR
  = ModIR Word8
  | MovIR Int
  | InpIR
  | OutIR
  | LopIR [IR]
  deriving Show

plusP :: Parser Instruction
plusP = do
  char '+'
  pure Inc

minusP :: Parser Instruction
minusP = do
  char '-'
  pure Dec

rightP :: Parser Instruction
rightP = do
  char '>'
  pure StepR

leftP :: Parser Instruction
leftP = do
  char '<'
  pure StepL

inP :: Parser Instruction
inP = do
  char ','
  pure In

outP :: Parser Instruction
outP = do
  char '.'
  pure Out

noopP :: Parser Instruction
noopP = do
  anySingleBut ']'
  pure Noop

instructionP :: Parser Instruction
instructionP = choice
  [ plusP
  , minusP
  , rightP
  , leftP
  , inP
  , outP
  , loopP
  , try noopP
  ]

loopP :: Parser Instruction
loopP = do
  char '['
  x <- many instructionP
  char ']'
  pure $ Loop x

instructionSP :: Parser [Instruction]
instructionSP = many instructionP

clearNoop :: [Instruction] -> [Instruction]
clearNoop []  = []
clearNoop (i : is) = case i of
  Loop x -> Loop (clearNoop x) : clearNoop is
  Noop -> clearNoop is
  x -> x : clearNoop is

parseIR :: [Instruction] -> [IR]
parseIR [] = []
parseIR (i : is) = case i of
  Inc -> ModIR 1 : parseIR is
  Dec -> ModIR 255 : parseIR is
  StepR -> MovIR 1 : parseIR is
  StepL  -> MovIR  (-1) : parseIR is
  In -> InpIR : parseIR is
  Out -> OutIR : parseIR is
  Loop x -> LopIR (parseIR x) : parseIR is
  Noop -> parseIR is

reduce :: [IR] -> [IR]
reduce [] = []
reduce (LopIR x : is) = LopIR (reduce x) : reduce is
reduce (ModIR x : ModIR y : is) = reduce (ModIR (x + y) : is)
reduce (MovIR x : MovIR y : is) = reduce (MovIR (x + y) : is)
reduce (i : is) = i : reduce is

unwrap :: Maybe [a] -> [a]
unwrap Nothing = []
unwrap (Just a) = a

runParse :: Text -> [IR]
runParse a = reduce . parseIR . clearNoop $ unwrap $ parseMaybe instructionSP a
