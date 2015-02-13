import Data.Char

main = do
    putStrLn "What's your name: "
    name <- getLine
    putStrLn ("Hello " ++ title name)

title :: String -> String
title str =
    next True str
    where
        -- next (capitalize) (string)
        next _ "" = ""
        next True  (x:rest) = toUpper x : next (x==' ') rest
        next False (x:rest) =         x : next (x==' ') rest
