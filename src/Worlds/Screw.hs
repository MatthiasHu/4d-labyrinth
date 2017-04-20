module Worlds.Screw
  ( screwWorld
  ) where

import Linear hiding (translation)
import Control.Lens

import SceneTO
import Transformation
import Color
import Objects.Cube
import Constraints.Vector


screwWorld :: (Monad m, SomeVector v, R3 v, Floating a) =>
  m (SceneTO v a, Transformation v a)
screwWorld = return (scene, eye)
  where
    scene = SceneFork $
      map (\(v, c) -> Transformed (translation $ zero & _xyz .~ v)
                      $ SceneObject (cube 0.5 c))
      [ (V3 1 (-1) (-4), green)
      , (V3 1 (-1) (-5), white)
      , (V3 0 (-1) (-5), red)
      , (V3 0 0    (-5), blue)
      ]
    eye = mempty
