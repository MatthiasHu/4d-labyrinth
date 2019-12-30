{-# LANGUAGE ScopedTypeVariables #-}

module Worlds.RandomTunnel
  ( randomTunnel
  , randomCavernousTunnel
  ) where

import Control.Monad.Random
import qualified Data.Set as Set
import Linear hiding (translation)
import Control.Lens

import Constraints.Scalar
import Constraints.Vector
import Worlds.RandomColorBox
import Transformation
import SceneTO
import Objects.Cube
import Object
import Color


randomTunnel :: (SomeVector v, MonadRandom m, SomeScalar a) =>
  Int -> m (SceneTO v a, Transformation v a)
randomTunnel n = randomCavernousTunnel n 0

randomCavernousTunnel :: (SomeVector v, MonadRandom m, SomeScalar a) =>
  Int -> Float -> m (SceneTO v a, Transformation v a)
randomCavernousTunnel n cavernousity = do
  path <- randomPath (pure (n-1)) (pure 1)
  let pathSet = Set.fromList path
  let allInnerPositions = sequenceA (pure [1..n-1])
  cavernousSet <- Set.fromList <$> thinOut cavernousity allInnerPositions
  let isHollow = (`Set.notMember` (pathSet `Set.union` cavernousSet))
  tunnelBox <- randomColorBoxTunnel n isHollow (pure 1)
  let gem = Transformed
        (translation . pure . fromIntegral $ (n-1))
        (SceneObject $ cubeWithWireframe 0.2 & objectColor .~ white)
      scene = SceneFork [tunnelBox, gem]
  return (scene, translation (pure (-1)))

-- Randomly delete some elements of a list.
-- (p is the probability of staying in.)
thinOut :: (MonadRandom m) => Float -> [a] -> m [a]
thinOut p [] = return []
thinOut p (x:xs) = do
  pass <- (p >) <$> getRandom
  rest <- thinOut p xs
  if pass
  then return (x:rest)
  else return rest

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
