module Game (
		TileKind(Wall,Floor,Bound), Tile(Tile), World(World),
		wall, floor, bound,
		dirN, dirS, dirW, dirE, dirNW, dirNE, dirSW, dirSE,
		getTile, getAdjacentTiles, smoothMap,
		mapSection,
		glyph, color,
		worldSize, worldTiles,
		newWorld) where

	import Prelude hiding (floor)
	import Common
	import Console (Color(White, Black, Red))
	import System.Random (randomR, RandomGen)

	data TileKind = Wall | Floor | Bound deriving (Show, Eq)
	data Tile = Tile { kind :: TileKind, glyph :: Char, color :: Color } deriving (Show, Eq)

	wall  = Tile Wall  '#' White
	floor = Tile Floor '.' White
	bound = Tile Bound 'X' Red

	data World = World { worldSize :: Size, worldTiles :: [[Tile]] } deriving (Show)

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
		(Size wx wy) = worldSize w
		in World (Size wx wy) [[smoothTile w $ Pos x y
		     | x <- [0..(wx-1)]]
			 | y <- [0..(wy-1)]]

	mapSection tiles x y dx dy = (take dy) . (drop y) $ map ((take dx) . (drop x)) tiles

	newRow :: RandomGen g => Int -> g -> ([Tile], g)
	newRow dx g = nRandomLs dx [floor, wall] g

	newMap :: RandomGen g => Size -> g -> ([[Tile]], g)
	newMap size g = newMap' size g where
		newMap' (Size dx dy) g = if dy == 0 then ([], g) else (row:rows, g'') where
			(row , g' ) = newRow dx g
			(rows, g'') = newMap' (Size dx (dy-1)) g'

	newWorld size g = (World size m, g') where (m, g') = (newMap size g)
