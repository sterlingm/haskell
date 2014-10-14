import Data.List
import Test.QuickCheck

testWord :: String -> Bool
testWord word =
  (length word > 10) || (sort (genPermutations word) == sort (permuataions word))

genPermutations :: String -> [String]
genPermutations "" = [""]
genPermutations [a] = [[a]]
genPermutations xs = 
  concatMap startWith [0..length xs - 1]
  where
    startWith i = map(xs !! i :) $ genPermutations (front ++ back)
      where (front, _:back) = splitAt i xs


main = do
  verboseCheck testWord
