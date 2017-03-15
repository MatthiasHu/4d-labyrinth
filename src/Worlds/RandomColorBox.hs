module Worlds.RandomColorBox
  ( randomColorBox
  ) where

import Control.Monad.Random
import Linear hiding (translation)

import SceneTO
import Objects.Cube
import Transformation
import Color


randomColorBox :: (MonadRandom m, Floating a) =>
  Int -> (V3 Int -> Bool)
  -> m (SceneTO a)
randomColorBox n p = fmap SceneFork $ sequence
  [ makeCube (V3 x y z) <$> randomColor
  | x <- [0..n], y <- [0..n], z <- [0..n], p (V3 x y z) ]
  where
    makeCube v c = Transformed (translation $ fmap fromIntegral v)
      . SceneObject $ cube c
