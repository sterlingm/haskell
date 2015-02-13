import System.Environment (getArgs)

-- Some practice with Haskell and differential equations


data Point = Point {px :: Float, py :: Float} deriving (Show)

euler :: (Float -> Float) -> Point -> Float -> Float -> Point 
euler f' (Point {px = x, py = y}) h x_end = 
  if x >= x_end then (Point x y) 
  else
    euler f' (Point (x+h) (((f' x) * h) + y)) h x_end



f_prime :: Float -> Float
f_prime x = 
    3*x*x + 2*x + 1


main = do
    args <- getArgs

    let a = (Point 1 1)

    print $ euler f_prime a 1 100000
