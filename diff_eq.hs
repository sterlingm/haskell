import System.Environment (getArgs)
import Numeric.GSL
import Numeric.LinearAlgebra
import Graphics.Plot

-- Some practice with Haskell and differential equations



-- Simple implementation of 1st-order euler method --
-- ************************************************************* --
-- ************************************************************* --

data Point = Point {px :: Float, py :: Float} deriving (Show)

-- Use Euler's method for approximating a first-order ode
euler :: (Point -> Float) -> Point -> Float -> Float -> Point 
euler f' p h x_end = 

  -- If terminating point is reached, return the point
  if (px p) >= x_end then p 

  -- Else, do a Euler step for the next point
  else
    euler f' (eulerStep f' p h) h x_end


-- Perform a single Euler step given a first-order derivative, Point, and
-- step size
eulerStep :: (Point -> Float) -> Point -> Float -> Point 
eulerStep f' p h =

  (Point x' y') 
  where 
    x' = (px p) + h 
    y' = ((f' p) * h) + (py p)



-- Example first-order differential 
f_prime :: Point -> Float
f_prime (Point {px = x, py = y}) = 
--  2 - (exp 1)**(-4*x) - 2*y 
  y - (1/2)*((exp 1)**(x/2))*sin(5*x) + 5*((exp 1)**(x/2))*cos(5*x)



-- ************************************************************* --
-- ************************************************************* --


-- Simple example using Numeric.GSL.ODE 
xdot t [x,v] = 
    [v, -0.95*x - 0.1*v]
ts = 
    linspace 100 (0,20)
sol = 
    odeSolve xdot [10,0] ts


main = do
    args <- getArgs

    let a = (Point 0 0)

    print $ euler f_prime a 0.01 3

    mplot(ts : toColumns sol)
