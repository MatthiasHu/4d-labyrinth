module Worlds.RandomColorBox
  ( randomColorBox
  , randomColorBoxTunnel
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
      . SceneObject $ cube 0.5 c
    positions = sequenceA (pure [0..n])

randomColorBoxTunnel :: (MonadRandom m, SomeVector v, Floating a) =>
  Int -> (v Int -> Bool) -> m (SceneTO v a)
randomColorBoxTunnel n p = randomColorBox n p'
  where
    p' v = p v && any not [ p v' | v' <- neighbours v ]

neighbours :: (SomeVector v) => v Int -> [v Int]
neighbours v = [ v ^+^ sign *^ e | e <- basis, sign <- [-1, 1] ]

