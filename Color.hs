module Color
  ( Color
  , red
  , green
  , blue
  , randomColor
  ) where

import Graphics.Rendering.OpenGL (GLfloat, Color3(..))
import Control.Monad.Random
import Control.Applicative

type Color = Color3 GLfloat


red, green, blue :: Color
red   = Color3 1 0 0
green = Color3 0 1 0
blue  = Color3 0 0 1


-- Map numbers between 0 and 1 to nice colors.
smoothColor :: GLfloat -> Color
smoothColor = interpolate list
  where
    list0 =
      [ Color3 1 0 0, Color3 1 1 0
      , Color3 0 1 0, Color3 0 1 1
      , Color3 0 0 1, Color3 1 0 1
      , Color3 1 0 0 ]
    n = fromIntegral $ length list0 - 1
    list = zip (map (/n) [0..]) list0
    interpolate l@((s, c):(t, d):_) x
      | s <= x && x <= t  = interpolateColors ((x-s)/(t-s)) c d
      | otherwise  = interpolate (tail l) x

interpolateColors :: GLfloat -> Color -> Color -> Color
interpolateColors t a b = liftA2 (+) (liftA (*t) b) (liftA (*(1-t)) a)

randomColor :: (MonadRandom m) => m Color
randomColor = smoothColor <$> getRandom
