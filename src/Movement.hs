module Movement
  ( movementInput
  , movement
  ) where

import Linear hiding (translation)
import Linear.Affine
import Control.Lens hiding (transform)
import Data.Monoid
import Graphics.Rendering.OpenGL (GLfloat)

import Transformation
import EventHandling


movementInput :: GLfloat -> Bool -> Bool -> State -> State
movementInput speed forward backward = movement $
  V3 0 0 1 ^* speed * (val forward - val backward)
  where
    val True = 1
    val False = 0

movement :: V3 GLfloat -> State -> State
movement v0_eye s = s & over eye (<> translation v0)
  where
    invEye = invert (s ^. eye)
    v0 = transform invEye v0_eye
    pos = P (translationPart invEye)
