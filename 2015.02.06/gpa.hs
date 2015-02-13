main = do
    putStrLn "Write in some scores to average: "
    scores_astext <- getLine
    let split_scores = words scores_astext
        for x y = map y x
        scores = for split_scores $ \s ->
            read s :: Double
        avg = sum(scores) / fromIntegral(length(scores))
    putStrLn("Your GPA is: " ++ show avg )
