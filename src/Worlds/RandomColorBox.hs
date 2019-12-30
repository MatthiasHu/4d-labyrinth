module Worlds.RandomColorBox
  ( randomColorBox
  , randomColorBoxTunnel
  ) where

import Control.Monad.Random
import Linear hiding (translation)
import qualified Data.Set as Set
import Control.Lens

import SceneTO
import Objects.Cube
import Object
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
      . SceneObject $ cube 0.5 & objectColor .~ c
    positions = sequenceA (pure [0..n])

-- Leave out cubes that aren't reachable anyway.
randomColorBoxTunnel :: (MonadRandom m, SomeVector v, Floating a) =>
  Int -> (v Int -> Bool) -> (v Int) -> m (SceneTO v a)
randomColorBoxTunnel n p innerPoint = randomColorBox n p'
  where
    reachableHoles = floodHollow p innerPoint
    p' v =    v `Set.notMember` reachableHoles
           && any (`Set.member` reachableHoles) (neighbours v)

floodHollow :: (SomeVector v) => (v Int -> Bool) -> v Int -> Set.Set (v Int)
floodHollow isWall start = go Set.empty (Set.singleton start)
  where
    go old current =
      if null current
      then old
      else
        let old' = old `Set.union` current
        in go old' (neighboursSet current Set.\\ old')
    neighboursSet =
        Set.filter (not . isWall)
      . foldMap (Set.fromList . neighbours)

neighbours :: (SomeVector v) => v Int -> [v Int]
neighbours v = [ v ^+^ sign *^ e | e <- basis, sign <- [-1, 1] ]
