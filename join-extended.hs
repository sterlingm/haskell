import Data.Tuple
import Data.Char -- for isSpace
import System.Environment -- getArgs
import Data.List -- for dropWhileEnd
import Control.Monad -- for when

-- | Find date of event
findDate :: String -> [(String, String)] -> String
findDate event table = 
    fst $ head $ filter (\(_, x) -> event == x) table

-- | Get a list of all tuples with same date
findEvents :: String -> [(String, String)] -> [(String, String)]
findEvents event table =
    filter (\x -> (fst x) == (findDate event table)) table

-- | Get events as a list of strings
relatedEvents :: String -> [(String, String)] -> [String]
relatedEvents event table = 
    map (snd) (findEvents event table)

-- | Remove space at the ends
trim :: String -> String
trim = (dropWhile isSpace) . (dropWhileEnd isSpace)

-- | Take the contents of a file and parse it into a table
-- it should look roughly like this:
-- May 5 : 553 – The Second Council of Constantinople begins.
-- So split by lines, then at the first colon in each line and
-- trim spaces of each end.
parseEvents :: String -> [(String, String)]
parseEvents = 
    (map trimEach) . (map splitColons) . lines
    where
        -- Split at the first colon (leaving it on the second half)
        splitColons = break (==':')
        -- Trash the colon. Trim each half.
        trimEach (date, event) =
            (trim date, trim (tail event))

-- | Take relatedEvents, but format the resulting list nicer by putting each item on a newline.
play :: [(String, String)] -> String -> String
play table query = 
    concat $ intersperse "\n" $ relatedEvents (trim query) table

-- | Read the extended data from a table
-- Welcome to imperative programming in Haskell
main = do
    -- Get the filename (IO). Must use <- before using it
    args <- getArgs
    when (null args) 
       (error "usage: join-extended <table>")
    let events_filename : _ = args    
    
    -- Read the file (more IO). Another <-
    table_string <- readFile events_filename
    let table = parseEvents table_string
    
    -- Show related events
    forever $ do
        l <- getLine
        putStrLn $ play table l

