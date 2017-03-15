module Objects.Tree
  ( tree
  ) where

import Linear hiding (translation)
import Data.Monoid

import Objects.Stick
import Objects.Tetrahedron
import Color
import SceneTO
import Transformation


tree :: (Floating a) => SceneTO a
tree = SceneFork
  [ SceneObject $ stick 4 0.15 h red
  , Transformed
      (translation (V3 0 0 h) <> rotation _xy (tau/8))
    . SceneObject $ tetrahedron 0.8 green
  ]
  where
    h = 1.5
    tau = 2*pi
