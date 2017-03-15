module SceneTO
  ( SceneTO
  , Scene(..)
  , sceneObjects
  ) where

import Scene
import Transformation
import Object


-- Specialization of Scene to Transfomation a and Object a.
type SceneTO a = Scene (Transformation a) (Object a)
