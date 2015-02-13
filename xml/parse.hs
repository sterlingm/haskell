{-# LANGUAGE OverloadedStrings #-}
import Text.XML hiding (readFile)
import qualified Text.XML as X
import qualified Filesystem.Path.CurrentOS as OS
import Text.XML.Lens
import Control.Lens
import System.Environment

main = do
    -- Boilerplate: get the filename, print it out
    filename : _ <- getArgs
    putStrLn $ "Reading document " ++ filename
    
    -- Read and parse the XML. Now it's a DOM-like tree format
    -- This is fine but digging deep in it takes wayyyy too much code
    doc <- X.readFile def (OS.decodeString filename)
    
    let for x y = map y x  -- to continue the left to right pipeline
    let kv_pairs = for (doc ^. root ^.. entire . named "Detail" . plate) (\x -> (x^.localName, x^.text))
    print kv_pairs