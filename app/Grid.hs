module Grid where

import Data.Vector (Vector)
import Data.Vector qualified as V

type Coords = (Int, Int)

data Grid a = Grid
    { items :: Vector a
    , rowCount :: Int
    , colCount :: Int
    }
    deriving stock (Show)

instance Functor Grid where
    fmap :: (a -> b) -> Grid a -> Grid b
    fmap fn g = g{items = V.map fn g.items}

fromList :: Int -> Int -> [a] -> Grid a
fromList rowCount' colCount' items' =
    Grid
        { rowCount = rowCount'
        , colCount = colCount'
        , items = V.fromList items'
        }

(!) :: Grid a -> Coords -> a
(!) g c = g.items V.! coordsToIndex g c

(!?) :: Grid a -> Coords -> Maybe a
(!?) g c = g.items V.!? coordsToIndex g c

indexToCoords :: Grid a -> Int -> Coords
indexToCoords g index = (index `mod` g.colCount, index `div` g.colCount)

coordsToIndex :: Grid a -> Coords -> Int
coordsToIndex g (x, y) = y * g.colCount + x

areCoordsInBounds :: Grid a -> Coords -> Bool
areCoordsInBounds g (x, y) = x >= 0 && x < g.colCount && y >= 0 && y < g.rowCount

neighbourCoords :: Grid a -> Int -> Coords -> [Coords]
neighbourCoords g size (x, y) =
    let allAreaCoords = [(xx, yy) | yy <- [y - 1 .. y + 1], xx <- [(x - 1) .. (x + size)]]
        inBoundsAreaCoords = filter (areCoordsInBounds g) allAreaCoords
        areOutsideCoords (cx, cy) = y /= cy || cx < x || cx >= x + size
     in filter areOutsideCoords inBoundsAreaCoords
