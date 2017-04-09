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


movementInput :: GLfloat -> Bool -> Bool -> State -> State
movementInput speed forward backward = movement $
  V3 0 0 (-1) ^* speed * (val forward - val backward)
  where
    val True = 1
    val False = 0

movement :: V3 GLfloat -> State -> State
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
