module Common (Pos(Pos), Size(Size), zeroPos) where
	data Pos = Pos Int Int deriving (Show, Eq)
	zeroPos = Pos 0 0
	data Size = Size Int Int deriving (Show, Eq)

