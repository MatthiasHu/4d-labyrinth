{-# LANGUAGE ScopedTypeVariables #-}

module Worlds.RandomTunnel
  ( randomTunnel
  ) where

import Control.Monad.Random
import qualified Data.Set as Set
import Linear hiding (translation)

import Constraints.Scalar
import Constraints.Vector
import Worlds.RandomColorBox
import Transformation
import SceneTO
import Objects.Octahedron
import Color


randomTunnel :: (MonadRandom m, SomeScalar a) =>
  Int -> m (SceneTO V3 a, Transformation V3 a)
randomTunnel n = do
  path <- randomPath (pure (n-1)) (pure 1)
  tunnelBox <- randomColorBox n (`Set.notMember` (Set.fromList path))
  let crystal = Transformed
        (translation . pure . fromIntegral $ (n-1))
        (SceneObject $ octahedron 0.3 white)
      scene = SceneFork [tunnelBox, crystal]
  return (scene, translation (pure (-1)))

-- A random monotonous path connecting two points.
randomPath :: forall v m.
  (SomeVector v, MonadRandom m) =>
  v Int -> v Int -> m [v Int]
randomPath a b = if null dirs then return [a]
  else do
    d <- uniform dirs
    let a' = a ^+^ d ^* (signum $ (b ^-^ a) `dot` d)
    rest <- randomPath a' b
    return (a:rest)
  where
    dirs :: [v Int]
    dirs = filter (\e -> a `dot` e /= b `dot` e) basis
