module Worlds.RandomScene
  ( randomScene
  ) where

import SceneTO
import Transformation
import Color
import Objects.Cube
import Object

import Linear hiding (translation)
import Control.Lens


randomScene :: (Monad m, Floating a) =>
  m (SceneTO V3 a, Transformation V3 a)
randomScene = return (scene, eye)
  where
    scene = Transformed (translation $ V3 1 0 (-4))
      $ SceneObject (cube 1.0 & objectColor .~ red)
    eye = mempty
