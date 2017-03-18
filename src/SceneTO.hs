module SceneTO
  ( SceneTO
  , Scene(..)
  , sceneObjects
  , transformedSceneObjects
  ) where

import Scene
import Transformation
import Object


-- Specialization of Scene to Transfomation a and Object a.
type SceneTO a = Scene (Transformation a) (Object a)

transformedSceneObjects :: (Num a) => SceneTO a -> [Object a]
transformedSceneObjects = map (uncurry transform) . sceneObjects
