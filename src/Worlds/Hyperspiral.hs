module Worlds.Hyperspiral
  ( hyperspiral
  , hyperspiral1
  ) where

import Linear hiding (translation)
import Control.Lens
import Data.Monoid

import Constraints.Scalar
import Constraints.Vector
import Transformation
import SceneTO
import Objects.Cube
import Color


hyperspiral :: (SomeVector v, R3 v, SomeScalar a) =>
  Transformation v a -> Int -> (SceneTO v a, Transformation v a)
hyperspiral dt n = (scene, eye)
  where
    eye = (translation $ zero & _z .~ (-2))
       <> (rotation _xz pi)
    scene = SceneFork [ SceneObject $ cube 0.5 red
                      , Transformed dt (cubes n) ]
    cubes 0 = SceneObject (cube 0.5 blue)
    cubes i = SceneFork [ SceneObject (cube 0.5 white)
                        , Transformed dt (cubes (i-1)) ]

hyperspiral1 :: (SomeVector v, R4 v, SomeScalar a) =>
  Int -> (SceneTO v a, Transformation v a)
hyperspiral1 = hyperspiral step1

step1 :: (SomeVector v, R4 v, SomeScalar a) =>
  Transformation v a
step1 = mempty
     <> rotation _xz 0.123
     <> rotation _xw 0.133
     <> rotation _zw 0.199
     <> rotation _xy 0.238
     <> translation (zero & _z .~ 1.2)
