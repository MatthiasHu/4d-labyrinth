module Objects.Stick
  ( stick
  ) where

import Linear
import Linear.Affine

import Object
import Color


stick :: (Floating a) => Int -> a -> a -> Color -> Object V3 a
stick n r l c = Object (P (V3 0 0 (l/2))) (norm $ V2 (l/2) r) $
  [ Face c (map P
              [ bot i     , bot (i+1)
              , top (i+1) , top i     ])
      (withZ 0 (dir (i+0.5)))
      faceDist
  | i <- is ]
  ++ [ Face c (map (P . bot) is) (V3 0 0 (-1)) 0
     , Face c (map (P . top) is) (V3 0 0 1   ) l ]
  where
    bot i = withZ 0 (r *^ dir i)
    top i = withZ l (r *^ dir i)
    is = map fromIntegral [0..(n-1)]
    dir i = let phi = i*tau/fromIntegral n
            in V2 (cos phi) (sin phi)
    withZ z (V2 x y) = V3 x y z
    faceDist = r * cos (0.5 * tau/fromIntegral n)
    tau = 2*pi
