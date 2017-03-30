module CollisionObject
  ( collisionTimeScene
  , collisionIntervalObject
  ) where

import Linear
import Linear.Affine
import Data.Maybe (catMaybes)
import Control.Lens

import Constraints.Vector
import Constraints.Scalar
import Geometry.Ray
import Geometry.Interval
import Geometry.Collision
import Geometry.Hyperplane
import Object
import SceneTO


collisionTimeScene :: (SomeVector v, SomeScalar a) =>
  a -> Ray v a -> Interval a -> SceneTO v a -> Maybe a
collisionTimeScene margin ray scope scene =
  case ts of
    [] -> Nothing
    ts' -> Just $ minimum ts'
  where
    ts = filter (> lowerBound scope)
         . map lowerBound . catMaybes
         . map (collisionIntervalObject margin ray scope)
         $ relevantObjects
    relevantObjects =
      filter (inReach (fst ray) reach) $
        transformedSceneObjects scene
    reach = magnitude scope * norm (snd ray)

collisionIntervalObject :: (SomeVector v, Ord a, Fractional a) =>
  a -> Ray v a -> Interval a -> Object v a -> Maybe (Interval a)
collisionIntervalObject margin ray scope =
    collisionInterval ray scope
  . map (movePlane margin)
  . view (partsOf (objectFaces . each . facePlane))
