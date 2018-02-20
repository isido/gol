import Data.List (nub)

data Cell = Coord Integer Integer deriving (Eq, Show)

type World = [Cell]

neighbours :: Cell -> [Cell]
neighbours (Coord x y) = [
  Coord (x - 1) y,
  Coord (x + 1) y,
  Coord (x - 1) (y - 1),
  Coord x (y - 1),
  Coord (x + 1) (y - 1),
  Coord (x - 1) (y + 1),
  Coord x (y + 1),
  Coord (x + 1) (y + 1)
  ]

numberOfNeighbours :: World -> Cell -> Int
numberOfNeighbours world cell =
  length $ filter (\x -> x `elem` (neighbours cell)) world

potentialPlacesForNewCells :: World -> Cell -> [Cell]
potentialPlacesForNewCells world cell =
  filter (\x -> x `notElem` world) (neighbours cell)

nextStateForLivingCell :: World -> Cell -> Maybe Cell
nextStateForLivingCell world cell =
  case numberOfNeighbours world cell of
    2 -> Just cell
    3 -> Just cell
    _ -> Nothing

nextStateForPotentialCell :: World -> Cell -> Maybe Cell
nextStateForPotentialCell world cell =
  case numberOfNeighbours world cell of
    3 -> Just cell
    _ -> Nothing

tick :: World -> World
tick world =
  nub $ foldr (\c w -> case c of
            Just c' -> c' : w
            Nothing -> w) [] nextCells
  where
    potentialCells = nub (concatMap (potentialPlacesForNewCells world) world)
    nextCells = (map (nextStateForLivingCell world) world) ++ (map (nextStateForPotentialCell world) potentialCells)

--- seeds for testing

-- still lifes
block = [Coord 0 0, Coord 1 0, Coord 0 1, Coord 1 1]
beehive = [Coord 1 1, Coord 2 1, Coord 0 0, Coord 3 0, Coord 1 (-1), Coord 2 (-1)]

-- oscillators
blinker = [Coord 0 0, Coord 1 0, Coord 2 0]

-- spaceships
glider = [Coord 0 0, Coord 1 0, Coord 2 0, Coord 2 1, Coord 1 2]
