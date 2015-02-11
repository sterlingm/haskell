-- Some practice with Haskell and differential equations

data Point = Point {pointx :: Float, pointy :: Float} deriving (Show)

euler :: (Float -> Float) -> Point -> Float -> Float -> Point 
euler f' (Point {pointx = x, pointy = y}) h x_end = 
  if x == x_end then (Point x y) 
  else
    euler f' (Point (x+h) (((f' x) * h) + y)) h x_end



f_prime :: Float -> Float
f_prime x = 
    x + 2
