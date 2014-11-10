import System.Environment (getArgs)
import Control.Monad (when)
import Data.Traversable (for)
import Data.Maybe
import Data.Ord (comparing)
import Data.List (sortBy)
import Debug.Trace
import qualified Data.IntMap as I
import qualified Data.PQueue.Prio.Min as PQ

-- | Tiles
data Tile = Unvisited 
          | Start
          | Goal
          | Obstacle
          | Visited
          deriving (Show, Enum, Eq)
type Board = I.IntMap Tile
width = 5

-- | Read a board into an array.
-- The shape is obnoxious
parseBoard :: String -> Board
parseBoard str =
    I.fromList $ zip [0..] (map (toEnum.read) $ words str)


-- Now for the interesting part. Use an A* search to find a path.
-- 0 = Unvisited
-- 1 = Start
-- 2 = Goal
-- 3 = Obstacle
-- 4 = Visited
search :: Board -> Maybe [Int]
search original_board =
    -- Blacklist : visited
    -- Graylist : frontier
    searchStep (mark original_board start Unvisited) start
    where
        -- This gives the same function a nicer verbal syntax
        mark board idx state = I.insert idx state board
        
        -- Find the constants of the board: the start and goal
        findTile tl = fst $ I.findMin $ I.filter (==tl) original_board
        start = findTile Start
        goal = findTile Goal
        
        -- Make a simple manhattan distance to the goal the A* heuristic
        -- for this search
        heuristic = manhattan goal
        cartes i = (i `div` width, i `rem` width)
        manhattan :: Int -> Int -> Int
        manhattan i1 i2 = abs(a2-a1) + abs(b2-b1)
            where
                (a1, b1) = cartes i1
                (a2, b2) = cartes i2
        
        -- Find the neighboring nodes
        getNeighbors :: Int -> [Int]
        getNeighbors center =
            filter noWrap $ filter inside $ map (+center) [-width, -1, 1, width]
            where
                noWrap i  = manhattan center i == 1    -- The shortest path should be length 1
                inside i  = 0 < i && i < (width*width) -- It must not be off the board
        
        
        -- Find the next best step
        searchStep :: Board -> Int -> Maybe [Int]
        searchStep board current_tile_i =
            traceShow ("Opening " ++ show current_tile_i) $
            case board I.! current_tile_i of
                 Unvisited -> case mapMaybe (searchStep next_board) next_options of
                                   [] -> Nothing -- No successful paths
                                   x:_ -> Just $ current_tile_i : x -- This step followed by the first path that worked
                 Goal -> Just [current_tile_i] -- One path, no more steps
                 Obstacle -> Nothing -- Can't go here
                 Visited -> Nothing -- Already saw it
                 -- I think there may be a case where you should update "Visited" nodes
                 -- But that sounds hard
            where
                next_board = mark board current_tile_i Visited
                next_options = sortBy (comparing heuristic) $ getNeighbors current_tile_i

main = do
    -- Get the filename (IO). Must use <- before using it
    args <- getArgs
    when (null args) 
        (error "usage: join-extended <table>")
    let board_filename : _ = args
    contents <- readFile board_filename
    let board = parseBoard contents
    
    print $ search board
         