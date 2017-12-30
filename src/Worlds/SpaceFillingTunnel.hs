{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Worlds.SpaceFillingTunnel
  ( spaceFillingTunnel
  ) where

import Data.Foldable
import qualified Data.Set as Set
import Control.Monad.Random
import Linear hiding (translation)

import Constraints.Scalar
import Constraints.Vector
import Worlds.RandomColorBox
import Transformation
import SceneTO
import Worlds.SpaceFillingPath


spaceFillingTunnel :: forall m v a.
  (MonadRandom m, SomeVector v, SomeScalar a) =>
  Int -> m (SceneTO v a, Transformation v a)
spaceFillingTunnel iteration = (, translation (pure (-1))) <$> scene
  where
    scene = randomColorBoxTunnel (2^(iteration+1))
      ((`Set.notMember` pathSet) . toList)
    pathSet :: Set.Set [Int]
    pathSet = Set.fromList path
    path = map (map (+1)) $ stretchedSpaceFillingPath dim iteration
    dim = length (pure 0 :: v Int)
