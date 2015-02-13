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
    hist <- H.new :: IO (H.BasicHashTable Text Int)
    eachLine path $ \line -> do
        let breaks = ICU.breaks (ICU.breakWord ICU.Current) line
            texts = map ICU.brkBreak breaks
            words' = filter (not . T.all (\c -> c == ' ')) texts
        
        forM_ words' $ \word -> do
            count <- H.lookup hist word
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
        fix $ \me -> do
            done <- hIsEOF handle
            unless done $ do
                line <- hGetLine handle 
                lineProc line
                me

-- # Step two, find the popularity of sets
data  FPTree =  FPTree Text Int         [FPTree]
data MFPTree = MFPTree Text (IORef Int) [IORef MFPTree]

addItem :: [Text] -> FPTree -> IO ()
addItem items tree = do
    let ordered_items = sort items
    addOneTree 

main = do
    path:_ <- getArgs
    hist <- takeHistogram path
    summary <- H.toList hist
    print summary