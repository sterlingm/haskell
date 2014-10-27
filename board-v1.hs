-- This is intended to be a short demo of arrays in Haskell using Repa
-- Unlike Haskell lists, they are compact, fast, multidimensional and parallel
-- But they can be ugly to use.

import System.Environment (getArgs)
import Control.Monad (when)
import Data.Traversable (for)
import Data.Maybe
import Debug.Trace
import Data.Array.Repa hiding (map)     -- Make array indexing tidier
--import qualified Data.Array.Repa as R -- Everything but indexing goes in R


-- | Array, unboxed, 2 dimensional, Int.
-- The syntax is not as pretty as C, I know.
type Board = Array U DIM2 Int
width = 5
shape = Z :. width :. width

-- | Read a board into an array.
-- The shape is obnoxious
parseBoard :: String -> Board
parseBoard =
    fromListUnboxed shape . map read . words

-- | Find a piece on the board
-- Not as easy as it sounds because fmap and friends don't work on Repa
argfilter :: Int -> Board -> [DIM2]
argfilter piece board =
    concat $ for [0..width-1] $ \x ->
        concat $ for [0..width-1] $ \y ->
            traceShow (x, y) $ if board ! (Z :. x :. y) == piece
                then [(Z :. x :. y)]
                else []

-- Now for the interesting part. Use an A* search to find a path.
-- 1 = Start
-- 2 = Goal
-- 3 = Obstacle
--search :: Board -> [R.DIM2]
--search board =
    -- Blacklist : visited
    -- Graylist : frontier
--    where
--        searchStep :: [(Int, Int), (Int, Int)]
--        searchStep blackList grayList

main = do
    -- Get the filename (IO). Must use <- before using it
    args <- getArgs
    when (null args) 
        (error "usage: join-extended <table>")
    let board_filename : _ = args
    contents <- readFile board_filename
    let board = parseBoard contents
    
    print $ argfilter 1 board
         
