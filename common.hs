module Common (Pos(Pos), Size(Size), zeroPos,
			   upperLeft, clamp, clampPos, moveRectToFit,
			   nRandomRs, nRandomLs) where

	import System.Random (randomR, RandomGen)


	data Pos = Pos Int Int deriving (Show, Eq)
	zeroPos = Pos 0 0
	data Size = Size Int Int deriving (Show, Eq)

	upperLeft (Pos cx cy) (Size sx sy) = Pos (cx - sx `div` 2) (cy - sy `div` 2)
	clamp bx dx x = max bx $ min (dx + bx) x
	clampPos (Pos bx by) (Size dx dy) (Pos x y) = Pos (clamp bx (dx-1) x) (clamp by (dy-1) y)

	moveRectToFit (Pos x y) (Size dx dy) (Size boundX boundY) = let
		pos0 = upperLeft (Pos x y) (Size dx dy)
		in clampPos (Pos 0 0)  (Size (boundX - dx) (boundY - dy)) pos0


	nRandomRs :: RandomGen g => Int -> (Int,Int) -> g -> ([Int], g)
	nRandomRs 0 r g = ([], g)
	nRandomRs num r g = let
		(n,  g')  = randomR r g
		(ns, g'') = nRandomRs (num-1) r g'
		in (n:ns, g'')

	nRandomLs :: RandomGen g => Int -> [a] -> g -> ([a], g)
	nRandomLs n l g = (map (l !!) ns, g') where (ns, g') = nRandomRs n (0, (length l) - 1) g
