import UI.HSCurses.Curses as C
import UI.HSCurses.CursesHelper as H

data Size = Size Int Int
data Screen = Start | Win | Lose | Quit
data Game = Game { consoleSize :: Size, screen :: Screen }

drawUI :: Game -> IO()
drawUI (Game (Size sizeX sizeY) Start) = do
		C.mvWAddStr C.stdScr (sizeY `div` 2 - 1) (sizeX `div` 2 - 13) "Welcome to Caves of Haskell"
		C.mvWAddStr C.stdScr (sizeY `div` 2) (sizeX `div` 2 - 13)     ":-----------=o=-----------:"
		C.mvWAddStr C.stdScr (sizeY `div` 2 + 3) (sizeX `div` 2 - 20) "Press enter to win, anything else to lose."

drawUI (Game (Size sizeX sizeY) Win) = do
		C.mvWAddStr C.stdScr (sizeY `div` 2 - 1) (sizeX `div` 2 - 13) "Congratulations, you win!"
		C.mvWAddStr C.stdScr (sizeY `div` 2 + 3) (sizeX `div` 2 - 20) "Press any key to restart, escape to quit."

drawUI (Game (Size sizeX sizeY) Lose) = do
		C.mvWAddStr C.stdScr (sizeY `div` 2 - 1) (sizeX `div` 2 - 13) "Congratulations, you lose..."
		C.mvWAddStr C.stdScr (sizeY `div` 2 + 3) (sizeX `div` 2 - 20) "Press any key to restart, escape to quit."

updateUI game = do
	C.erase
	drawUI game

updateGame (Game size Start) (C.KeyChar '\r')    = Game size Win
updateGame (Game size Start) _                   = Game size Lose
updateGame (Game size Win)   (C.KeyChar '\ESC')  = Game size Quit
updateGame (Game size Win)   _                   = Game size Start
updateGame (Game size Lose)  (C.KeyChar '\ESC')  = Game size Quit
updateGame (Game size Lose)  _                   = Game size Start
updateGame game c                                = game


runGame :: Game -> IO()
runGame (Game size Quit) = do return ()
runGame game = do
	updateUI game
	C.refresh
	c <- C.getCh
	runGame (updateGame game c)

main = do
	H.start
	(sizeY, sizeX) <- C.scrSize
	runGame $ Game (Size sizeX sizeY) Start
	H.end
