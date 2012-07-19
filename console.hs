module Console (Console(Console), Console.Color(White, Black, Red),
				Console.start, stop, getConsole, consoleSize,
				clearScreen, updateScreen,
				fgColor, drawString,
				getKeyPress,
				Key(KeyUp, KeyDown, KeyRight, KeyLeft, KeyChar), escapeKey, enterKey) where
	import UI.HSCurses.Curses as C
	import UI.HSCurses.CursesHelper as H
	import Common (Pos(Pos), Size(Size))

	data Color = White | Black | Red deriving (Show, Eq)
	data Console = Console { size :: Size }

	fgColor White = H.WhiteF
	fgColor Black = H.BlackF
	fgColor Red   = H.RedF

	escapeKey = C.KeyChar '\ESC'
	enterKey = C.KeyChar '\r'

	getConsole :: IO Console
	getConsole = do
		(ty, tx) <- scrSize
		return $ Console $ Size tx ty

	consoleSize :: Console -> Size
	consoleSize = size

	drawString :: Console -> Pos -> String -> IO ()
	drawString (Console (Size tx ty)) (Pos x y) s = let
		l = length s in do
		if (x >= 0 && y >= 0 && (x + l) <= tx && y <= ty && (not $ y == 0 && (x+l) == tx))
			then C.mvWAddStr C.stdScr (ty - y - 1) x s 
			else do
				clearScreen
				C.mvWAddStr C.stdScr 0 0 ("Tried to print '" ++ s ++ "' (len:" ++ (show l) ++ ") @" ++ (show (Pos x y)) ++ " size:" ++ (show (Size tx ty)))
				updateScreen
				c <- getKeyPress
				return ()



	start = do
		H.start
		C.cursSet CursorInvisible

	stop = do
		H.end

	updateScreen = do C.refresh

	clearScreen = do C.erase

	getKeyPress = do C.getCh
