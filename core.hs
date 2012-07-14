import Prelude hiding (floor)
import System.Random (mkStdGen, randomR, getStdGen, StdGen, RandomGen, Random)
import UI.HSCurses.Curses as C
import UI.HSCurses.CursesHelper as H
import Data.Time.Clock (getCurrentTime)

data Pos = Pos Int Int
data Size = Size Int Int deriving (Show, Eq)
data Screen = Start | Play | Win | Lose | Quit deriving (Show)
data TileKind = Wall | Floor | Bound deriving (Show, Eq)
data Tile = Tile { kind :: TileKind, glyph :: Char, color :: H.ForegroundColor } deriving (Show, Eq)

wall  = Tile Wall  '#' H.WhiteF
floor = Tile Floor '.' H.WhiteF
bound = Tile Bound 'X' H.BlackF


data World = World { worldSize :: Size, worldTiles :: [[Tile]] } deriving (Show)
data Game = Game {
	consoleSize :: Size,
	world :: World,
	screen :: Screen, 
	gen :: StdGen}
	deriving (Show)

nRandomRs :: RandomGen g => Int -> (Int,Int) -> g -> ([Int], g)
nRandomRs 0 r g = ([], g)
nRandomRs num r g = let
	(n,  g')  = randomR r g
	(ns, g'') = nRandomRs (num-1) r g'
	in (n:ns, g'')

nRandomLs :: RandomGen g => Int -> [a] -> g -> ([a], g)
nRandomLs n l g = (map (l !!) ns, g') where (ns, g') = nRandomRs n (0, (length l) - 1) g

newRow :: RandomGen g => Int -> g -> ([Tile], g)
newRow dx g = nRandomLs dx [floor, wall] g

newMap :: RandomGen g => Size -> g -> ([[Tile]], g)
newMap size g = newMap' size g where
	newMap' (Size dx dy) g = if dy == 0 then ([], g) else (row:rows, g'') where
		(row , g' ) = newRow dx g
		(rows, g'') = newMap' (Size dx (dy-1)) g'

dirN (Pos x y) = Pos x (y+1)
dirS (Pos x y) = Pos x (y-1)
dirW (Pos x y) = Pos (x-1) y
dirE (Pos x y) = Pos (x+1) y
dirNE = dirN . dirE
dirNW = dirN . dirW
dirSE = dirS . dirE
dirSW = dirS . dirW

getTile w (Pos x y) = let
	tiles = worldTiles w
	(Size dx dy) = worldSize w
	in if x < 0 || y < 0 || x >= dx || y >= dy then bound else tiles !! y !! x

getAdjacentTiles w pos = map (\f -> getTile w $ f pos) [dirN, dirNE, dirE, dirSE, dirS, dirSW, dirW, dirNW]
smoothTile' t a = if (length . (filter (floor ==)) $ t:a) >= 5 then floor else wall
smoothTile w p = smoothTile' (getTile w p) (getAdjacentTiles w p)
smoothMap w = let
	tiles = worldTiles w
	(Size cx cy) = worldSize w
	in World (Size cx cy) [[smoothTile w $ Pos x y
	     | x <- [0..(cx-1)]]
		 | y <- [0..(cy-1)]]

drawTiles (row:rows) y = do
	C.mvWAddStr C.stdScr y 0 row
	drawTiles rows (y+1)
drawTiles [] _ = do return()

mapSection tiles x y dx dy = take dy $ map (take dx) tiles

drawMap (Game (Size cx cy) (World size tiles) _ _) = let
	glyphs = reverse $ map (map glyph) (mapSection tiles 0 0 cx (cy-1))
	in do drawTiles glyphs 0

worldRandoms w n ls = (w {gen = g'}, l) where (l, g') = nRandomLs n ls (gen w)
worldRandom w ls = (w {gen = g'}, l) where (l:_, g') = nRandomLs 1 ls (gen w)

newWorld size g = (World size m, g') where (m, g') = (newMap size g)
newGame conSize size g = Game conSize w Start g' where (w, g') = newWorld size g

drawUI :: Game -> IO()
drawUI (Game (Size sizeX sizeY) world Start _) = do
		C.mvWAddStr C.stdScr (sizeY `div` 2 - 1) (sizeX `div` 2 - 13) "Welcome to Caves of Haskell"
		C.mvWAddStr C.stdScr (sizeY `div` 2) (sizeX `div` 2 - 13)     ":-----------=o=-----------:"
		C.mvWAddStr C.stdScr (sizeY `div` 2 + 3) (sizeX `div` 2 - 20) "Press any key to start..."

drawUI (Game (Size sizeX sizeY) world Win _) = do
		C.mvWAddStr C.stdScr (sizeY `div` 2 - 1) (sizeX `div` 2 - 13) "Congratulations, you win!"
		C.mvWAddStr C.stdScr (sizeY `div` 2 + 3) (sizeX `div` 2 - 20) "Press any key to restart, escape to quit."

drawUI (Game (Size sizeX sizeY) world Lose _) = do
		C.mvWAddStr C.stdScr (sizeY `div` 2 - 1) (sizeX `div` 2 - 13) "Too bad, you lose..."
		C.mvWAddStr C.stdScr (sizeY `div` 2 + 3) (sizeX `div` 2 - 20) "Press any key to restart, escape to quit."

drawUI (Game (Size sizeX sizeY) world Play g) = do
	drawMap (Game (Size sizeX sizeY) world Play g)
	C.mvWAddStr C.stdScr (sizeY-1) 0 "Press enter to win, s to smooth the map or anything else to lose."


updateUI game = do
	C.erase
	drawUI game

updateGame (Game size w Start g) _                   = Game size w Play g
updateGame (Game size w Play g)  (C.KeyChar '\ESC')  = Game size w Lose g
updateGame (Game size w Play g)  (C.KeyChar 's')     = Game size (smoothMap w) Play g
updateGame (Game size w Play g)  _                   = Game size w Win g
updateGame (Game size w Win g)   (C.KeyChar '\ESC')  = Game size w Quit g
updateGame (Game size w Win g)   _                   = newGame size (worldSize w) g
updateGame (Game size w Lose g)  (C.KeyChar '\ESC')  = Game size w Quit g
updateGame (Game size w Lose g)  _                   = newGame size (worldSize w) g
updateGame game c                                    = game


runGame :: Game -> IO()
runGame (Game _ _ Quit _) = do return ()
runGame game = do
	updateUI game
	C.refresh
	c <- C.getCh
	runGame (updateGame game c)

main = do
	H.start
	C.cursSet CursorInvisible
	(sizeY, sizeX) <- C.scrSize
	g <- getStdGen
	runGame $ newGame (Size sizeX sizeY) (Size 160 50) g
	H.end

