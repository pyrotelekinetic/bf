data Tape a = Tape [a] a [a]

curElem :: Tape a -> a
curElem (Tape _ x _) = x

setElem :: a -> Tape a -> Tape a
setElem x (Tape prev _ next) = Tape prev x next

goLeft :: Tape a -> Tape a
goLeft (Tape (y : left) x right) = Tape left y (x : right)

goRight :: Tape a -> Tape a
goRight (Tape left x (y : right)) = Tape (x : left) y right

main = putStrLn "noerr"
