import Data.Char

main = do
    putStrLn "What's your name: "
    name <- getLine
    putStrLn ("Hello " ++ title name)

caps "" = ""
caps (x:rest) = toUpper x : caps rest

title :: String -> String
title str =
    nextChar True str
    where
        -- nextChar (capitalize) (string)
        nextChar _ "" = ""
        nextChar True  (x:rest) = toUpper x : nextChar (x==' ') rest
        nextChar False (x:rest) =         x : nextChar (x==' ') rest
