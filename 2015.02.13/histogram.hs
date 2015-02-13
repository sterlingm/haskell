{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (lines, words, putStrLn, putStr, getLine, words)
import System.IO (hIsEOF, withFile, IOMode(..))
import System.Environment (getArgs)
import Data.Text (Text)
import Data.List (sort)
import qualified Data.Text as T
import Data.Text.IO
import Control.Monad
import Data.Function (fix)
import qualified Data.HashTable.IO as H
import qualified Data.Text.ICU as ICU

-- # Step one, find the popularity of each unigram
--newtype Popularity = H.BasicHashTable Text Int

-- Count all of the words in a file
takeHistogram :: String -> IO (H.BasicHashTable Text Int)
takeHistogram path = do 
    -- Create a new empty histogram
    hist <- H.new :: IO (H.BasicHashTable Text Int)
    -- for line in file:
    eachLine path $ \line -> do
        -- Break the line into "break" objects
        let breaks = ICU.breaks (ICU.breakWord ICU.Current) line
            -- Extract the text from each break
            texts = map ICU.brkBreak breaks
            -- Remove the words that are empty space
            words' = filter (not . T.all (\c -> c == ' ')) texts
        -- For every word in the line
        forM_ words' $ \word -> do
            -- Get the count
            count <- H.lookup hist word
            -- Increment it and put it back
            H.insert hist word $ maybe 1 (+1) count
    return hist


-- Run an action on every line of a file
eachLine :: String -> (Text -> IO ()) -> IO ()
eachLine path lineProc = do
    -- This is an example of a higher order monadic function
    -- It will run an IO action once for every line in a file, which is
    -- found at the given path
    -- In most languages, like Scala or C++, all functions are actually this type.
    -- You can see how it incourages sharing state, which can be faster or slower.
    withFile path ReadWriteMode $ \handle ->
        -- Fix passes the function to itself so it can recurse.
        fix $ \me -> do
            done <- hIsEOF handle
            unless done $ do
                line <- hGetLine handle 
                lineProc line
                me

main = do
    path:_ <- getArgs
    hist <- takeHistogram path
    summary <- H.toList hist
    print summary