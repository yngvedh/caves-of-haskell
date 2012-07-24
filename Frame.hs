module Frame (Screen(Start,Play,Win,Lose,Quit)) where

	import Common
	import Game (World)

	data Screen = Start
			| Play { world :: World, center :: Pos }
			| Win
			| Lose
			| Quit deriving (Show)


{-
	data Frame T = Frame {
		pos :: Pos,
		size :: Size,
		draw :: -> ()
		tick :: Key -> T
	}
-}