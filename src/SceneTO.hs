module SceneTO
  ( SceneTO
  , Scene(..)
  , rawSceneObjects
  , sceneObjects
  , transformedSceneObjects
  , colorScene
  ) where

import Control.Lens hiding (transform)

import Constraints.Vector
import Scene
import Transformation
import Object
import Color


-- Specialization of Scene to Transfomations and Objects.
type SceneTO v a = Scene (Transformation v a) (Object v a)

transformedSceneObjects :: (SomeVector v, Num a) =>
  SceneTO v a -> [Object v a]
transformedSceneObjects = map (uncurry transform) . sceneObjects

colorScene :: Color -> SceneTO v a -> SceneTO v a
colorScene c = rawSceneObjects . objectColor .~ c
