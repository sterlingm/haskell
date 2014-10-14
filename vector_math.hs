module Mathask where

import qualified Data.Vector.Unboxed as V

instance (V.Unbox n, Num n) => Num (V.Vector n) where
    (+) = V.zipWith(+)
    (-) = V.zipWith(-)
    (*) = V.zipWith(*)
    signum = V.map signum
    abs = V.map abs
    negate = V.map negate
    fromInteger = V.singleton . fromIntegral
