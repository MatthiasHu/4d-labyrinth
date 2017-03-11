module Objects.Stick
  ( stick
  ) where

import Linear
import Linear.Affine

import Object
import Color


stick :: (Floating a) => Int -> a -> a -> Color -> Object a
stick n r l c = Object $
  [ Face c (map P
              [ withZ 0 (r *^ dir i) , withZ 0 (r *^ dir (i+1))
              , withZ l (r *^ dir (i+1)) , withZ l (r *^ dir i) ])
      (withZ 0 (dir (i+0.5)))
  | i <- map fromIntegral [0..(n-1)] ]
  where
    dir i = let phi = i*tau/fromIntegral n
            in V2 (cos phi) (sin phi)
    withZ z (V2 x y) = V3 x y z
    tau = 2*pi
