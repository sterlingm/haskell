deleteItem :: Eq a => a -> [a] -> [a]
deleteItem _ [] = []
deleteItem query (mat:rest) = 
    if(query == mat)
      then deleteItem query rest
      else mat : deleteItem query rest

deleteUsingFilter = filter (\x -> not (x == 5) )
deleteUsingFilter_ list = filter (\x -> not (x == 5) ) list

main = do
  putStrLn "Example 1: "  
  print $ deleteItem 5 [1,2,5,3,4]

  putStrLn "Example 2: "
  print $ filter (\x -> not (x == 5) ) [1,2,5,3,4]

