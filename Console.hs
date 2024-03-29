module Console (
	Console,
	Cell (..), Buffer, Color (..),
	newBuffer,
	drawChar, drawString, drawColorChar,
	(>>!),
	updateConsole,
	getConsole, releaseConsole,
	getConsoleSize,
	nextEvent, Event (..)
	) where

	import Common
	import Control.Monad.State
	import qualified Graphics.Vty as Vty
	import Graphics.Vty (with_fore_color, with_style)
	import Prelude hiding (Left, Right)

	data Color = Black | White | Red
	data Cell = NonEmpty Char Color | Empty
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

	attrFromColor Black = Vty.def_attr `with_fore_color` Vty.black
	attrFromColor White = Vty.def_attr `with_fore_color` Vty.bright_white
	attrFromColor Red   = Vty.def_attr `with_fore_color` Vty.bright_red

	nextEvent :: Console -> IO Event
	nextEvent con = do
		e <- Vty.next_event $ vty con
		return $ case e of (Vty.EvKey k _)    -> inputFromEvent k
		                   (Vty.EvResize x y) -> Resize $ Size (fromEnum x) (fromEnum y)
		                   _                  -> Unknown

	putCells' :: Buffer -> Pos -> [Cell] -> Buffer
	putCells' (Buffer size buf) (Pos x y) cs = Buffer size $ t ++ (newRow : b)
							  where
							  	t = take y buf
							  	row = buf !! y
							  	b = drop (y+1) buf
							  	newRow = l ++ cs ++ r where
							  		l = take x row
							  		r = drop (x+(length cs)) row

	putCells :: Pos -> [Cell] -> State Buffer ()
	putCells pos cells = state $ \buffer -> ((), putCells' buffer pos cells)

	putCell :: Pos -> Cell -> State Buffer ()
	putCell pos cell = putCells pos [cell]

	imageFromCell :: Cell -> Vty.Image
	imageFromCell (NonEmpty c color) = Vty.char (attrFromColor color) c
	imageFromCell Empty        = Vty.char Vty.def_attr ' '
	imageFromRow row = Vty.horiz_cat $ map imageFromCell row
	imageFromBuffer (Buffer _ buf) = Vty.vert_cat $ map imageFromRow $ reverse buf

	updateConsole :: Console -> Buffer -> IO ()
	updateConsole (Console vty) buffer= do
		Vty.update vty $ Vty.pic_for_image $ imageFromBuffer buffer

	drawColorChar pos color c = putCell pos (NonEmpty c color)
	drawChar :: Pos -> Char -> State Buffer ()
	drawChar pos c = drawColorChar pos White c


	drawString :: Pos -> String -> State Buffer ()
	drawString pos s = putCells pos $ map (\x -> NonEmpty x White) s

	(>>!) :: Buffer -> (Buffer -> Buffer) -> Buffer
	buf >>! f = f buf

	drawPalette :: Vty.Image
	drawPalette = Vty.vert_cat $ map drawRow $ map (8*) [0..14] where
		drawRow n = Vty.horiz_cat $ map (\c -> Vty.char (Vty.def_attr `with_fore_color` (Vty.Color240 $ n + c)) '#') [0..15]

	getConsole :: IO Console
	getConsole = do
		vty <- Vty.mkVty
		Vty.hide_cursor $ Vty.terminal vty
--		Vty.update vty $ Vty.pic_for_image $ drawPalette
--		e <- Vty.next_event vty
		return $ Console vty

	getConsoleSize :: Console -> IO Size
	getConsoleSize (Console vty) = do
		Vty.DisplayRegion sx sy <- Vty.display_bounds $ Vty.terminal vty
		return $ Size (fromEnum sx) (fromEnum sy)

	releaseConsole :: Console -> IO ()
	releaseConsole con = do Vty.shutdown $ vty con
