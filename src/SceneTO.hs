module SceneTO
  ( SceneTO
  , Scene(..)
  , sceneObjects
  , transformedSceneObjects
  ) where

import Constraints.Vector
import Scene
import Transformation
import Object


-- Specialization of Scene to Transfomations and Objects.
type SceneTO v a = Scene (Transformation v a) (Object v a)

transformedSceneObjects :: (SomeVector v, Num a) =>
  SceneTO v a -> [Object v a]
transformedSceneObjects = map (uncurry transform) . sceneObjects
