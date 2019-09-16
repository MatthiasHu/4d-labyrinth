module Worlds.RandomColorBox
  ( randomColorBox
  , randomColorBoxTunnel
  ) where

import Control.Monad.Random
import Linear hiding (translation)
import Control.Lens

import SceneTO
import Objects.Cube
import Object
import Transformation
import Worlds.Tessellation as T
import Color
import Constraints.Vector


randomColorBox :: (MonadRandom m, SomeVector v, Floating a) =>
  Int -> (v Int -> Bool) -> m (SceneTO v a)
randomColorBox n p = fmap SceneFork $ sequence
  [ makeCube pos <$> randomColor
  | pos <- positions, p pos ]
  where
    makeCube v c = Transformed (translation $ fmap fromIntegral v)
      . SceneObject $ cube 0.5 & objectColor .~ c
    positions = sequenceA (pure [0..n])

randomColorBoxTunnel :: (MonadRandom m, SomeVector v, Floating a) =>
  Int -> (v Int -> Bool) -> m (SceneTO v a)
randomColorBoxTunnel n p = randomColorBox n p'
  where
    p' v = p v && any not [ p v' | v' <- neighbours cubes v ]
