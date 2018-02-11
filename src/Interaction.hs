module Interaction
  ( placeObject
  ) where

import Control.Lens hiding (translation)
import Linear hiding (translation)
import Data.Monoid

import State
import Transformation
import Scene
import Constraints.Vector


placeObject :: (SomeVector v, R3 v) => State v -> State v
placeObject state = case state ^. placeableObject of
    Nothing -> state
    Just o -> state &
      scene .~ SceneFork [new o, state ^. scene]
  where
    new = Transformed (invert (state ^. eye) <> inFront)

inFront :: (SomeVector v, R3 v, Fractional a) => Transformation v a
inFront = translation (zero & _z .~ (-0.5))
