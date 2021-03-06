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
import Objects.Cube
import Color


randomTunnel :: (SomeVector v, MonadRandom m, SomeScalar a) =>
  Int -> m (SceneTO v a, Transformation v a)
randomTunnel n = do
  path <- randomPath (pure (n-1)) (pure 1)
  let pathSet = Set.fromList path
  tunnelBox <- randomColorBoxTunnel n (`Set.notMember` pathSet)
  let gem = Transformed
        (translation . pure . fromIntegral $ (n-1))
        (SceneObject $ cube 0.2 white)
      scene = SceneFork [tunnelBox, gem]
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
