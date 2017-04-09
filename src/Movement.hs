module Movement
  ( movementInput
  , movement
  ) where

import Graphics.Rendering.OpenGL (GLfloat)
import Linear hiding (translation)
import Linear.Affine
import Control.Lens hiding (transform)
import Data.Monoid
import Data.Maybe (fromMaybe)

import State
import Transformation
import CollisionObject
import SceneTO
import Geometry.Interval
import Constraints.Vector


movementInput :: (SomeVector v, R3 v) =>
  GLfloat -> Bool -> Bool -> State v -> State v
movementInput speed forward backward = movement $
  (zero & _z .~ (-1)) ^* (speed * (val forward - val backward))
  where
    val True = 1
    val False = 0

movement :: (SomeVector v) => v GLfloat -> State v -> State v
movement v0_eye s = s & eye %~ (<> translation ((-1) * t *^ v0))
  where
    invEye = invert (s ^. eye)
    v0 = transform invEye v0_eye
    pos = P (translationPart invEye)
    t = fromMaybe 1.0 $
          collisionTimeScene margin ray scope (s ^. scene)
    margin = 0.01
    ray = (pos, v0)
    scope = interval (-0.1) 1
