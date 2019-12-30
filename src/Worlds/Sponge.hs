module Worlds.Sponge
  ( spongeTunnel
  ) where

import Control.Monad.Random
import qualified Data.Set as Set
import Control.Lens

import Constraints.Scalar
import Constraints.Vector
import Worlds.RandomColorBox
import Transformation
import SceneTO
import Objects.Cube
import Object
import Color


spongeTunnel :: (SomeVector v, MonadRandom m, SomeScalar a) =>
  Int -> m (SceneTO v a, Transformation v a)
spongeTunnel n = do
  s <- spongeTunnelSet n
  tunnelBox <- randomColorBoxTunnel n (`Set.notMember` s) (pure 1)
  let gem = Transformed
        (translation . pure . fromIntegral $ (n-1))
        (SceneObject $ cubeWithWireframe 0.2 & objectColor .~ white)
      scene = SceneFork [tunnelBox, gem]
  return (scene, translation (pure (-1)))

spongeTunnelSet :: (SomeVector v, MonadRandom m) =>
  Int -> m (Set.Set (v Int))
spongeTunnelSet n = go (Set.singleton start)
  where
    go s =
      if finish `Set.member` floodHollow (`Set.notMember` s) start
      then return s
      else do
        new <- uniform allInnerPositions
        go (Set.insert new s)
    start = (pure 1)
    finish = (pure (n-1))
    allInnerPositions = sequenceA (pure [1..n-1])
