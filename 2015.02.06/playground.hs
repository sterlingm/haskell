import Data.Char

main = do


    putStrLn "What's Your Sign? :"
    astro_sign <- getLine
    putStrLn ("The Magis 8 Ball will now read your horoscope this week for the sign of: " ++ caps astro_sign)
    putStrLn ("Because you are such a good " ++ title astro_sign ++" you will be rewarded with 500 Drew Carey Points \n")
    
    
    putStrLn "This is a good place to tell your lucky lottery numbers. What are your favorite numbers? :"
    fave_nums_txt <- getLine
    let fave_nums = [ read s :: Double | s <- words fave_nums_txt ]
    let avg = sum(fave_nums) / fromIntegral(length(fave_nums))
    putStrLn("Your Lucky Lottery Number is: " ++ show avg)
    

caps :: String -> String
caps "" = ""
caps (x:rest) = toUpper x : caps rest
    
title :: String -> String
title str = 
    next True str 
    where
        -- next (capitalize) (string)
       next _ "" = ""
       next True (x:rest) = toUpper x : next (x==' ') rest
       next False (x:rest) = x:next (x==' ') rest
       
    