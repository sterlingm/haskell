
data Point = Point {pointx :: Float, pointy :: Float} deriving (Show)

euler :: Point -> Float -> Float -> Point 
euler (Point {pointx = x, pointy = y}) h x_end = 
  if x == x_end then (Point x y) 
  else
    euler (Point (x+h) (((f_prime x) * h) + y)) h x_end



f_prime :: Float -> Float
f_prime x = 
    x + 2
