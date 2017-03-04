module Worlds.RandomScene
  ( randomScene
  ) where

import Object
import Scene
import Transformation
import Color
import Objects.Cube

import Linear hiding (translation)


randomScene :: (Monad m, Floating a) =>
  m (Scene (Transformation a) (Object a), Transformation a)
randomScene = return (scene, eye)
  where
    scene = Transformed (translation $ V3 1 0 (-4))
      $ SceneObject (cube red)
    eye = mempty
