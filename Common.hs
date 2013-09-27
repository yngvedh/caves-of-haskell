module Common (Pos(Pos), Size(Size), zeroPos,
			   upperLeft, clamp, clampPos, moveRectToFit,
			   nRandomLs, randomL) where

	import System.Random (randomR, RandomGen)
	import Control.Monad (replicateM)
	import Control.Monad.State (State, get, put)


	data Pos = Pos Int Int deriving (Show, Eq)
	zeroPos = Pos 0 0
	data Size = Size Int Int deriving (Show, Eq)

	upperLeft (Pos cx cy) (Size sx sy) = Pos (cx - sx `div` 2) (cy - sy `div` 2)
	clamp bx dx x = max bx $ min (dx + bx) x
	clampPos (Pos bx by) (Size dx dy) (Pos x y) = Pos (clamp bx (dx-1) x) (clamp by (dy-1) y)

	moveRectToFit (Pos x y) (Size dx dy) (Size boundX boundY) = let
		pos0 = upperLeft (Pos x y) (Size dx dy)
		in clampPos (Pos 0 0)  (Size (boundX - dx + 1) (boundY - dy + 2)) pos0


	randomR' :: RandomGen g => (Int,Int) -> State g Int
	randomR' r = do
		g <- get
		let (n, g') = randomR r g
		put g'
		return n

	randomL :: RandomGen g => [a] -> State g a
	randomL as = do
		n <- randomR' (0, (length as) - 1)
		return $ as !! n

	nRandomLs :: RandomGen g => Int -> [a] -> State g [a]
	nRandomLs n as = replicateM n $ randomL as
