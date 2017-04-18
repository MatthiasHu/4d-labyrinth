{-# LANGUAGE ScopedTypeVariables #-}

module Worlds.Grid
  ( gridWorld
  ) where

import Control.Monad.Random
import Data.Monoid

import Worlds.RandomColorBox
import Transformation
import SceneTO
import Constraints.Vector


gridWorld :: forall m v a.
  (MonadRandom m, SomeVector v, Floating a) =>
  Int -> Int -> m (SceneTO v a, Transformation v a)
gridWorld size density = do
  scene <- randomColorBox size ((<density) . count even)
  return (scene, mempty)

count :: (Foldable t) => (a -> Bool) -> t a -> Int
count f = getSum . foldMap (Sum . boolToInt . f)
  where
    boolToInt False = 0
    boolToInt True  = 1
