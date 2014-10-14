split :: (Ord a) => [a] -> ([a],[a])
split [] = ([],[])
split x = if length x == 1 then (x,[]) else splitAt ((length x) `div` 2) x


--merge xs [] = xs
--merge [] ys = ys
--merge (x:xs) (y:ys) = if x <= y then x : merge xs (y:ys)
--	else y : merge ys (x:xs)


--let a = [1,2,3,4,5,6]
--let (b,c) = split a
--merge b c

--mergeSort x = merge (split x)
--
--
--
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort inputs = 
    merge (mergeSort beginning) (mergeSort end)
    where
        
      (beginning, end) = splitAt pivot inputs
      pivot = length inputs `div` 2

      merge [] ys = ys
      merge xs [] = xs
      merge (x:xs) (y:ys) =
        if x < y
          then x : (merge xs (y:ys))
          else y : (merge (x:xs) (ys))
