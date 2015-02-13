{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (lines, words, putStrLn, putStr, getLine, words, hGetContents)
import System.IO (hIsEOF, withFile, IOMode(..))
import System.Environment (getArgs)
import Data.Text.Lazy (Text)
import Data.List (sort, foldl')
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.IO
import Control.Monad
import Data.Function (fix)
import qualified Data.Map as H

main = do
    path:_ <- getArgs
    withFile path ReadWriteMode $ \handle -> do
        -- Read the file
        content <- hGetContents handle
        -- Convert it to lines
        let content_lines = T.lines content
            -- Convert each line to words
            lines_words = map T.words content_lines
            -- Make a function to count the words in a line
            count_line = foldl' (\hist word -> H.insertWith' (+) word 1 hist) H.empty
            -- Count the words in all the lines
            lines_hists = map count_line lines_words
            -- Merge the results from all of the counts
            finished_hist = H.unionsWith (+) lines_hists
        -- Print out the finished result
        print $ H.toList finished_hist