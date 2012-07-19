import Prelude hiding (floor)
import System.Random (getStdGen, StdGen, RandomGen)
import Control.Monad (when)
import Common
import Frame
import Game
import Console as Con

data Game = Game {
	console :: Con.Console,
	screen :: Screen, 
	gen :: StdGen}

drawTiles con (row:rows) y = do
	Con.drawString con (Pos 0 y) row
	drawTiles con rows (y-1)
drawTiles _ [] _ = do return()

drawCrosshair con (Pos x y) worldSize = let
	(Size sx sy) = consoleSize con
	(Pos x0 y0) = moveRectToFit (Pos x y) (Size sx sy) worldSize
	(Pos x' y') = Pos (x - x0) (y - y0)
	in do when (x >= 0 && x' < sx && y' >= 0 && y' < (sy - 1)) $ Con.drawString con (Pos x' y') "X"

drawMap con center (World (Size wx wy) tiles) = let
	conSize = consoleSize con
	(Pos x0 y0) = moveRectToFit center conSize (Size wx wy)
	(Size sx sy) = conSize
	glyphs = reverse $ map (map glyph) (mapSection tiles x0 y0 sx $ sy - 1)
	in do drawTiles con glyphs $ sy-1

newPlay :: RandomGen g => Size -> g -> (Screen, g)
newPlay (Size wx wy) g = ((Play w c), g') where 
		(w, g') = newWorld (Size wx wy) g
		c = Pos (wx `div` 2) (wy `div` 2)

drawUI :: Game -> IO()
drawUI (Game con Start _) = let
		(Size sizeX sizeY) = consoleSize con
		in do
			Con.drawString con (Pos (sizeX `div` 2 - 13) (sizeY `div` 2 - 1))	"Welcome to Caves of Haskell"
			Con.drawString con (Pos (sizeX `div` 2 - 13) (sizeY `div` 2))		":-----------=o=-----------:"
			Con.drawString con (Pos (sizeX `div` 2 - 20) (sizeY `div` 2 + 3))	"Press any key to start..."
			Con.drawString con zeroPos (show (Size sizeX sizeY))

drawUI (Game con Win _) = let
		(Size sizeX sizeY) = consoleSize con
		in do
			Con.drawString con (Pos (sizeX `div` 2 - 13) (sizeY `div` 2 - 1)) "Congratulations, you win!"
			Con.drawString con (Pos (sizeX `div` 2 - 20) (sizeY `div` 2 + 3)) "Press any key to restart, escape to quit."

drawUI (Game con Lose _) = let
		(Size sizeX sizeY) = consoleSize con
		in do
			Con.drawString con (Pos (sizeX `div` 2 - 13) (sizeY `div` 2 - 1)) "Too bad, you lose..."
			Con.drawString con (Pos (sizeX `div` 2 - 20) (sizeY `div` 2 + 3)) "Press any key to restart, escape to quit."

drawUI (Game con (Play world center) _) = do
	drawMap con center world
	drawCrosshair con center (worldSize world) 
	Con.drawString con zeroPos $ show center ++ " -> " ++ show (moveRectToFit center (consoleSize con) (worldSize world))


updateUI game = do
	Con.clearScreen
	drawUI game

updateGame (Game size Start g) _                       		= Game size p g' where (p, g') = newPlay (Size 160 50) g
updateGame (Game size (Play w c) g)  (Con.KeyChar '\ESC')	= Game size Lose g
updateGame (Game size (Play w c) g)  (Con.KeyChar 's')   	= Game size (Play w' c) g where w' = smoothMap w
updateGame (Game size (Play w c) g)  (Con.KeyUp)			= Game size (Play w c') g where c' = clampPos zeroPos (worldSize w) $ dirN c
updateGame (Game size (Play w c) g)  (Con.KeyDown)			= Game size (Play w c') g where c' = clampPos zeroPos (worldSize w) $ dirS c
updateGame (Game size (Play w c) g)  (Con.KeyLeft)			= Game size (Play w c') g where c' = clampPos zeroPos (worldSize w) $ dirW c
updateGame (Game size (Play w c) g)  (Con.KeyRight)			= Game size (Play w c') g where c' = clampPos zeroPos (worldSize w) $ dirE c
updateGame (Game size (Play _ _) g)  _                  	= Game size Win g
updateGame (Game size Win  g)  (Con.KeyChar '\ESC')  		= Game size Quit g
updateGame (Game size Win  g)  _                   			= Game size Start g
updateGame (Game size Lose g)  (Con.KeyChar '\ESC')  		= Game size Quit g
updateGame (Game size Lose g)  _                   			= Game size Start g
updateGame game _                                    		= game


runGame :: Game -> IO()
runGame (Game _ Quit _) = do return ()
runGame game = do
	updateUI game
	Con.updateScreen
	c <- Con.getKeyPress
	runGame (updateGame game c)

main = do
	Con.start
	con <- Con.getConsole
	g <- getStdGen
	runGame $ Game con Start g
	Con.stop

