module CollisionObject
  ( collisionTimeScene
  , collisionIntervalObject
  ) where

import Linear
import Linear.Affine
import Data.Maybe (catMaybes)
import Control.Lens

import Collision
import Object
import SceneTO
import Interval


type Ray a = (Point V3 a, V3 a)

collisionTimeScene :: (Ord a, Fractional a) =>
  a -> Ray a -> Interval a -> SceneTO a -> Maybe a
collisionTimeScene margin ray scope scene =
  case ts of
    [] -> Nothing
    ts' -> Just $ minimum ts'
  where
    ts = filter (> lowerBound scope)
         . map lowerBound . catMaybes
         . map (collisionIntervalObject margin ray scope)
         $ transformedSceneObjects scene

collisionIntervalObject :: (Ord a, Fractional a) =>
  a -> Ray a -> Interval a -> Object a -> Maybe (Interval a)
collisionIntervalObject margin ray scope =
  collisionInterval ray scope
  . map strengthenInequality
  . boundingInequalities
  where
    strengthenInequality = _2 +~ margin
