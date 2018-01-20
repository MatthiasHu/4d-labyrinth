module Movement
  ( movementInput
  , movement
  ) where

import Linear hiding (translation)
import Linear.Affine
import Control.Lens hiding (transform)
import qualified Control.Monad.State as MS
import Data.Monoid
import Data.Maybe (fromMaybe)

import State
import Transformation
import CollisionObject
import SceneTO
import Geometry.Interval
import Constraints.Vector


movementInput :: (SomeVector v, R3 v) =>
  Scalar -> [Ordering] -> State v -> State v
movementInput speed inputs = movement $
  fillVector (map val inputs ++ repeat 0) ^* speed
  where
    val LT = -1
    val EQ = 0
    val GT = 1

-- Fill a vector with values from a list,
-- traversing the vector structure with a MS.State action.
-- The list must be long enough.
fillVector :: (SomeVector v) => [a] -> v a
fillVector = MS.evalState . sequenceA . pure . MS.state $
  \(x:xs) -> (x, xs)

movement :: (SomeVector v) => v Scalar -> State v -> State v
movement v0_eye s = s & eye %~ (<> translation ((-1) * t *^ v0))
  where
    invEye = invert (s ^. eye)
    v0 = transform invEye v0_eye
    pos = P (translationPart invEye)
    t = fromMaybe 1.0 $
          collisionTimeScene margin ray scope (s ^. scene)
    margin = 0.02
    ray = (pos, v0)
    scope = interval (-0.1) 1
