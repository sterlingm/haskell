import Control.Monad.ST
import Test.QuickCheck

monAdd x y = runST $ do         -- Valid but crazy add
    a <- return x
    return $ a + y

monBad x y = runST $ do         -- Broken, crazy add
    a <- return $ x `rem` 5
    return $ a + y

-- Label the type so that it knows what to generate
addsRight :: (Int -> Int -> Int) -> Int -> Int -> Bool
addsRight f x y =       x `f` y == x + y

main = do -- Run the unit test
    print "monAdd";     verboseCheck $ addsRight monAdd
    print "monBad";     verboseCheck $ addsRight monBad