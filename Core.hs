import Prelude hiding (floor, Left, Right)
import System.Random (getStdGen, StdGen, RandomGen)
import Control.Monad (when)
import Common
import Frame
import Game
import Console

data Game = Game {
	screen :: Screen, 
	gen :: StdGen}

drawTileRow pos@(Pos x y) (t:ts) buf = drawChar pos t $ drawTileRow (Pos (x+1) y) ts buf
drawTileRow _ [] buf = buf

drawTileRows pos@(Pos x y) (r:rs) buf = drawTileRow pos r $ drawTileRows (Pos x $ y - 1) rs buf
drawTileRows _ [] buf = buf

drawMap conSize@(Size sx sy) center (World (Size wx wy) tiles) buf = let
	(Pos x0 y0) = moveRectToFit center conSize (Size wx wy)
	glyphs = reverse $ map (map glyph) (mapSection tiles x0 y0 sx $ sy - 1)
	in drawTileRows (Pos 0 $ sy-1) glyphs buf

drawCrosshair (Size sx sy) (Pos x y) worldSize buf = let
	(Pos x0 y0) = moveRectToFit (Pos x y) (Size sx sy) worldSize
	pos = Pos (x - x0) (y - y0 + 1)
	in drawChar pos 'X' buf


newPlay :: RandomGen g => Size -> g -> (Screen, g)
newPlay (Size wx wy) g = ((Play w c), g') where 
		(w, g') = newWorld (Size wx wy) g
		c = Pos (wx `div` 2) (wy `div` 2)


messageAt :: Pos -> [String] -> Buffer -> Buffer
messageAt pos@(Pos x y) (msg:msgs) buf = messageAt (Pos x $ y - 1) msgs $ drawString pos msg buf
messageAt _ [] buf = buf

messageWithInstruction :: Size -> [String] -> String -> Buffer
messageWithInstruction size@(Size tx ty) messages instruction = 
								messageAt (Pos msgX msgY) messages $
								messageAt zeroPos [instruction] $
								newBuffer size
								where
									msgWidth = foldl max 0 $ map length messages
									msgX = (tx - msgWidth) `div` 2
									msgHeight = length messages
									msgY = (ty - msgHeight) `div` 2

drawUI :: Size -> Game -> Buffer
drawUI size (Game Start _) = messageWithInstruction size
			["Welcome to Caves of Haskell", ":-----------=o=-----------:"] "Press any key to start..."

drawUI size (Game Win _) = messageWithInstruction size ["Congratulations, you win!"] "Press any key to restart, escape to quit."
drawUI size (Game Lose _) = messageWithInstruction size ["Too bad, you lose..."] "Press any key to restart, escape to quit."

drawUI size@(Size tx ty) (Game (Play world center) _) = 
	drawCrosshair size center (worldSize world) $
	drawMap size center world $
	drawString zeroPos (show center ++ " -> " ++ show (moveRectToFit center (Size tx ty) (worldSize world))) $
	newBuffer size

updateGame (Game Start g) _					= Game p g' where (p, g') = newPlay (Size 160 50) g
updateGame (Game (Play w c) g) Cancel		= Game Lose g
updateGame (Game (Play w c) g) (Key 's')	= Game (Play w' c) g where w' = smoothMap w
updateGame (Game (Play w c) g) Up			= Game (Play w c') g where c' = clampPos zeroPos (worldSize w) $ dirN c
updateGame (Game (Play w c) g) Down			= Game (Play w c') g where c' = clampPos zeroPos (worldSize w) $ dirS c
updateGame (Game (Play w c) g) Left			= Game (Play w c') g where c' = clampPos zeroPos (worldSize w) $ dirW c
updateGame (Game (Play w c) g) Right		= Game (Play w c') g where c' = clampPos zeroPos (worldSize w) $ dirE c
updateGame (Game (Play _ _) g) _			= Game Win g
updateGame (Game Win  g) Cancel				= Game Quit g
updateGame (Game Win  g) _					= Game Start g
updateGame (Game Lose g) Cancel				= Game Quit g
updateGame (Game Lose g) _					= Game Start g
updateGame game _							= game


runGame :: Console -> Size -> Game -> IO()
runGame _ _ (Game Quit _) = do return ()
runGame con size game = do
	updateConsole con $ drawUI size game
	e <- nextEvent con
	case e of
	    Resize size'  -> runGame con size' game
	    Unknown       -> runGame con size game
	    otherwise     -> runGame con size $ updateGame game e

main = do
	g <- getStdGen
	con <- getConsole
	size <- getConsoleSize con
	runGame con size $ Game Start g
	releaseConsole con

