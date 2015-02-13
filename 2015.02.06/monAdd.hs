import Control.Monad.ST
import Test.QuickCheck

-- Monadic but referentially transparent subject
monAdd x y = runST $ do
    a <- return x
    b <- return (a + y)
    return b

-- Label the type so that it knows what to generate
addsRight :: Int -> Int -> Bool
addsRight x y =
    x `monAdd` y == x + y

-- Run the unit test
main = 
    verboseCheck addsRight