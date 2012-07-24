module Console (
	Console,
	Cell (..), Buffer,
	newBuffer,
	drawChar, drawString,
	updateConsole,
	getConsole, releaseConsole,
	getConsoleSize,
	nextEvent, Event (..)
	) where

	import Common
	import qualified Graphics.Vty as Vty
	import Prelude hiding (Left, Right)

	data Cell = NonEmpty Char | Empty
	data Buffer = Buffer Size [[Cell]]
	data Console = Console { vty :: Vty.Vty }
	data Event = Unknown | Up | Down | Left | Right | Cancel | Key Char | Resize Size

	newBuffer size@(Size x y) = Buffer size $ replicate y $ replicate x Empty

	inputFromEvent Vty.KEsc			= Cancel
	inputFromEvent Vty.KUp			= Up
	inputFromEvent Vty.KDown		= Down
	inputFromEvent Vty.KLeft		= Left
	inputFromEvent Vty.KRight		= Right
	inputFromEvent (Vty.KASCII c)	= Key c

	nextEvent :: Console -> IO Event
	nextEvent con = do
		e <- Vty.next_event $ vty con
		return $ case e of (Vty.EvKey k _)    -> inputFromEvent k
		                   (Vty.EvResize x y) -> Resize $ Size (fromEnum x) (fromEnum y)
		                   _                  -> Unknown

	putCells :: Buffer -> Pos -> [Cell] -> Buffer
	putCells (Buffer size buf) (Pos x y) cs = Buffer size $ t ++ (newRow : b)
							  where
							  	t = take y buf
							  	row = buf !! y
							  	b = drop (y+1) buf
							  	newRow = l ++ cs ++ r where
							  		l = take x row
							  		r = drop (x+(length cs)) row

	putCell :: Buffer -> Pos -> Cell -> Buffer
	putCell buf pos c = putCells buf pos [c]

	imageFromCell :: Cell -> Vty.Image
	imageFromCell (NonEmpty c) = Vty.char Vty.def_attr c
	imageFromCell Empty        = Vty.char Vty.def_attr ' '
	imageFromRow row = Vty.horiz_cat $ map imageFromCell row
	imageFromBuffer (Buffer _ buf) = Vty.vert_cat $ map imageFromRow $ reverse buf

	updateConsole :: Console -> Buffer -> IO ()
	updateConsole (Console vty) buffer= do
		Vty.update vty $ Vty.pic_for_image $ imageFromBuffer buffer

	drawChar :: Pos -> Char -> Buffer -> Buffer
	drawChar pos c buffer = putCell buffer pos (NonEmpty c)

	drawString :: Pos -> String -> Buffer -> Buffer
	drawString pos s buffer = putCells buffer pos $ map NonEmpty s

	getConsole :: IO Console
	getConsole = do
		vty <- Vty.mkVty
		Vty.hide_cursor $ Vty.terminal vty
		return $ Console vty

	getConsoleSize :: Console -> IO Size
	getConsoleSize (Console vty) = do
		Vty.DisplayRegion sx sy <- Vty.display_bounds $ Vty.terminal vty
		return $ Size (fromEnum sx) (fromEnum sy)

	releaseConsole :: Console -> IO ()
	releaseConsole con = do Vty.shutdown $ vty con
