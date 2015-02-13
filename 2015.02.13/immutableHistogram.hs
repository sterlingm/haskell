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
        content <- hGetContents handle
        let content_lines = T.lines content
            lines_words = map T.words content_lines
            count_line = foldl' (\hist word -> H.insertWith' (+) word 1 hist) H.empty
            lines_hists = map count_line lines_words
            finished_hist = H.unionsWith (+) lines_hists
        
        print $ H.toList finished_hist