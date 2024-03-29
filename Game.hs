module Game (
		TileKind(Wall,Floor,Bound), Tile(Tile), World(World),
		wall, floor, bound,
		dirN, dirS, dirW, dirE, dirNW, dirNE, dirSW, dirSE,
		getTile, getAdjacentTiles, smoothMap,
		mapSection,
		glyph,
		worldSize, worldTiles,
		newWorld, testWorld) where

	import Prelude hiding (floor)
	import Common
	import System.Random (randomR, RandomGen)
	import Control.Monad.State (State)
	import Control.Monad (replicateM)

	data TileKind = Wall | Floor | Bound deriving (Show, Eq)
	data Tile = Tile { kind :: TileKind, glyph :: Char} deriving (Show, Eq)

	wall  = Tile Wall  '#'
	floor = Tile Floor '.'
	bound = Tile Bound 'X'

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

	newRow :: RandomGen g => Int -> State g [Tile]
	newRow dx = nRandomLs dx [floor, wall]

	newMap :: RandomGen g => Size -> State g [[Tile]]
	newMap (Size dx dy) = replicateM dx $ newRow dy

	newWorld :: RandomGen g => Size -> State g World
	newWorld size = newMap size >>= \m -> return $ World size m

	testWorld (Size x y) g = (World (Size x y) tiles, g) where
		tiles = (replicate x wall) : (replicate (y-2) (wall : (replicate (x-2) floor) ++ [wall])) ++ [replicate x wall]
