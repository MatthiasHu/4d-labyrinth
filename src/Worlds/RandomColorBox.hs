module Worlds.RandomColorBox
  ( randomColorBox
  ) where

import Control.Monad.Random
import Linear hiding (translation)

import SceneTO
import Objects.Cube
import Transformation
import Color
import Constraints.Vector


randomColorBox :: (MonadRandom m, SomeVector v, Floating a) =>
  Int -> (v Int -> Bool) -> m (SceneTO v a)
randomColorBox n p = fmap SceneFork $ sequence
  [ makeCube pos <$> randomColor
  | pos <- positions, p pos ]
  where
    makeCube v c = Transformed (translation $ fmap fromIntegral v)
      . SceneObject $ cube c
    positions = sequenceA (pure [0..n])
