{-# LANGUAGE TupleSections #-}

module Worlds.OrthogonalPlanes
  ( orthogonalPlanes
  ) where

import Linear
import Control.Lens
import Control.Monad.Random

import Worlds.RandomColorBox
import SceneTO
import Transformation


orthogonalPlanes :: (MonadRandom m, Floating a) =>
  Int -> m (SceneTO V4 a, Transformation V4 a)
orthogonalPlanes n = (, mempty) <$> randomColorBox n
  (\v ->    (v^._x == (n `div` 2) && v^._y == (n `div` 2))
         || (v^._z == (n `div` 2) && v^._w == (n `div` 2)) )
