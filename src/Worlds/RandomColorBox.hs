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
  | pos <- linearCombinations [0..n] basis, p pos ]
  where
    makeCube v c = Transformed (translation $ fmap fromIntegral v)
      . SceneObject $ cube c

linearCombinations :: (Additive v, Num a) => [a] -> [v a] -> [v a]
linearCombinations factors (v:vs) = do
  i <- factors
  vs' <- linearCombinations factors vs
  return (i*^v ^+^ vs')
linearCombinations factors [] = [zero]
