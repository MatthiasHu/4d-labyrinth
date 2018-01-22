{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Worlds.SpaceFillingTunnel
  ( spaceFillingTunnel
  ) where

import Data.Foldable
import qualified Data.Set as Set
import Control.Monad.Random
import Linear hiding (translation)
import Control.Lens

import Constraints.Scalar
import Constraints.Vector
import Worlds.RandomColorBox
import Transformation
import SceneTO
import Worlds.SpaceFillingPath
import Objects.Cube
import Color


spaceFillingTunnel :: forall m v a.
  (MonadRandom m, SomeVector v, R4 v, SomeScalar a) =>
  Int -> m (SceneTO v a, Transformation v a)
spaceFillingTunnel iteration =
    (, translation (pure (-1)))
  . (\s -> SceneFork [s, gem])
  <$> scene
  where
    scene = randomColorBoxTunnel (2^(iteration+1))
      ((`Set.notMember` pathSet) . toList)
    pathSet :: Set.Set [Int]
    pathSet = Set.fromList path
    path = map (map (+1)) $ stretchedSpaceFillingPath dim iteration
    dim = length (pure 0 :: v Int)
    gem = Transformed
      (translation $ pure 1 ^+^ (zero & _x .~ 2^iteration))
      (SceneObject $ cube 0.2 white)
