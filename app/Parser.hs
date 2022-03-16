module Parser (IR (..), runParse) where

import Data.Word (Word8)

data Instruction
	= Inc
	| Dec
	| StepR
	| StepL
	| In
	| Out
	| LoopO
	| LoopC
	| Noop
	deriving Eq

data IR
	= ModIR Word8
	| MovIR Int
	| InpIR
	| OutIR
	| LopIR [IR]
	deriving Show

parse :: Char -> Instruction
parse = \case
	'+' -> Inc
	'-' -> Dec
	'>' -> StepR
	'<' -> StepL
	',' -> In
	'.' -> Out
	'[' -> LoopO
	']' -> LoopC
	_ -> Noop

clearNoop :: [Instruction] -> [Instruction]
clearNoop = filter (/= Noop)

condense :: Eq a => [a] -> [(Int, a)]
condense a = go (1, a)
	where
	go :: Eq a => (Int, [a]) -> [(Int, a)]
	go (_, []) = []
	go (i, a : []) = [(i, a)]
	go (i, a1 : a2 : as)
		| a1 == a2 = go (i + 1, a1 : as)
		| otherwise = (i, a1) : go (1, a2 : as)

reduce :: [(Int, Instruction)] -> [IR]
reduce [] = []
reduce (x : xs) = case x of
	(n, Inc) -> ModIR (toEnum n) : reduce xs
	(n, Dec) -> ModIR (- toEnum n) : reduce xs
	(n, StepR) -> MovIR n : reduce xs
	(n, StepL) -> MovIR (-n) : reduce xs
	(n, In) -> replicate n InpIR ++ reduce xs
	(n, Out) -> replicate n OutIR ++ reduce xs
	(n, LoopO) -> LopIR (reduce a) : reduce b
		where
		(a, b) = getLoop xs
	(n, LoopC) -> reduce xs

getLoop :: [(Int, Instruction)] -> ([(Int, Instruction)], [(Int, Instruction)])
getLoop [] = ([], [])
getLoop (x : xs) = case x of
	(1, LoopC) -> ([], xs)
	(n, LoopC) -> ([], (n - 1, LoopC) : xs)
	y -> (\(a, (b, c)) -> (a : b, c)) (y, getLoop xs)

reduceMore :: [IR] -> [IR]
reduceMore [] = []
reduceMore (ModIR x : ModIR y : is) = reduceMore (ModIR (x + y) : is)
reduceMore (MovIR x : MovIR y : is) = reduceMore (MovIR (x + y) : is)
reduceMore (i : is) = i : reduceMore is

runParse = reduceMore . reduce . condense . clearNoop . (map $ parse)
