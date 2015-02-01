-- Data.List gives us (among many others):
-- sort :: [x] -> [x]
-- permutations :: [x] -> [[x]]
import Data.List

-- Text.QuickCheck gives us (also among many others):
-- verboseCheck :: Testable prop => prop -> IO ()
import Test.QuickCheck

-- Keep in mind that String is a synonym for [Char]

-- This is our Unit Test.
-- It takes one example, and returns whether it passed the example.
testWord :: String -> Bool
testWord word =
  -- Either the word is too long (meaning it will take too long)
  (length word > 10)
   -- Or the subject of the test (genPermutations)
   -- will be the same as the reference implementation (permutations)
   -- We sort them because we don't think the order we generate them matters
  || (sort (genPermutations word) == sort (permutations word))


-- Defined the function in parts
--genPermutations :: String -> [String]
genPermutations a = [a]       -- Only one way to permute an empty set
genPermutations [a] = [[a]]     -- Also only one way to permute a singleton
genPermutations xs =            -- Everything else is reducable to the first two
  -- For i as every index in xs, concatenating the results of (startsWith i)
  concatMap startWith [0..length xs - 1]
  where
    -- Move the item at i to the front and permute the rest
    startWith i = map (xs !! i :) $ genPermutations (front ++ back)
      where (front, _:back) = splitAt i xs


main = verboseCheck testWord
