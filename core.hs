import UI.HSCurses.Curses as C
import UI.HSCurses.CursesHelper as H

main = do
	H.start
	(sizeY, sizeX) <- C.scrSize
	C.mvWAddStr C.stdScr (sizeY `div` 2 - 1) (sizeX `div` 2 - 13) "Welcome to Caves of Haskell"
	C.mvWAddStr C.stdScr (sizeY `div` 2) (sizeX `div` 2 - 13)     ":-----------=o=-----------:"
	C.mvWAddStr C.stdScr (sizeY `div` 2 + 3) (sizeX `div` 2 - 12) "Press any key to exit..."
	C.refresh
	c <- C.getCh
	H.end
