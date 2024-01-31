module Interpreter where

import Data.Word (Word8)
import Data.Char (ord, chr)

import Parser (IR (..))

data Zipper a = Zipper [a] a [a]
  deriving Show

type Tape = Zipper Word8

getElem :: Zipper a -> a
getElem (Zipper _ o _) = o

setElem :: a -> Zipper a -> Zipper a
setElem o (Zipper q _ p) = Zipper q o p

modElem :: (a -> a) -> Zipper a -> Zipper a
modElem f (Zipper q o p) = setElem (f o) $ Zipper q o p

goLeft :: Zipper a -> Zipper a
goLeft (Zipper (y : left) x right) = Zipper left y (x : right)
goLeft (Zipper [] _ _) = undefined -- Should be an error

goRight :: Zipper a -> Zipper a
goRight (Zipper left x (y : right)) = Zipper (x : left) y right
goRight (Zipper _ _ []) = undefined -- Should add more cells

shiftTape :: Int -> Zipper a -> Zipper a
shiftTape 0 a = a
shiftTape n z
  | n > 0 = r n z
  | otherwise = l n z
    where
    r :: Int -> Zipper a -> Zipper a
    r 0 a = a
    r n t = r (n - 1) $ goRight t
    l :: Int -> Zipper a -> Zipper a
    l 0 a = a
    l n t = l (n + 1) $ goLeft t

initTape :: Int -> Tape
initTape n = Zipper [] 0 (replicate (n - 1) 0)

tape :: Tape
tape = Zipper [] 5 (5 : 65 : replicate 7 0)

exec :: Tape -> [IR] -> IO Tape
exec t [] = pure t
exec t (i : is) = case i of
  ModIR n -> exec (modElem (+ n) t) is
  MovIR n -> exec (shiftTape n t) is
  InpIR -> do
    o <- getChar
    exec (setElem (toEnum . ord $ o) t) is
  OutIR -> do
    putChar $ chr . fromIntegral $ getElem t
    exec t is
  LopIR x -> case getElem t of
    0 -> exec t is
    _ -> do
      t' <- exec t x
      exec t' (i : is)

execDebug :: Tape -> [IR] -> IO Tape
execDebug t [] = do
  print t
  putStrLn "[]"
  pure t
execDebug t (i : is) = putStrLn (show t ++ "\t" ++ show (i : is)) >> case i of
  ModIR n -> exec (modElem (+ n) t) is
  MovIR n -> exec (shiftTape n t) is
  InpIR -> do
    o <- getChar
    exec (setElem (toEnum . ord $ o) t) is
  OutIR -> do
    putChar $ chr . fromIntegral $ getElem t
    exec t is
  LopIR x -> case getElem t of
    0 -> exec t is
    _ -> do
      t' <- exec t x
      exec t' (i : is)
