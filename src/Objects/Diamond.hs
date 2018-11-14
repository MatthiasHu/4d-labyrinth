{-# LANGUAGE ScopedTypeVariables #-}

module Objects.Diamond
  ( diamond
  ) where

import Linear
import Linear.Affine
import Control.Lens
import Data.List

import Object
import Color
import Geometry.Hyperplane
import Geometry.Combinatorics
import Constraints.Vector


diamond :: forall v a. (SomeVector v, Floating a, Ord a) =>
  a -> Object v a
diamond cubeInnerRadius =
  simpleObject (max 2 (sqrt dim) * cubeInnerRadius)
    (diamondFaces cubeInnerRadius)
  & objectInnerRadius .~ (sqrt 3 * cubeInnerRadius)
  where
    dim = sum (pure 1 :: v a)

diamondFaces :: (SomeVector v, Floating a) => a -> [Hyperplane v a]
diamondFaces r = do
  [v1, v2] <- choose' 2 basis
  s1 <- [-1, 1]
  s2 <- [-1, 1]
  return $ Hyperplane ((1/sqrt 2) *^ (s1*^v1 ^+^ s2*^v2)) (sqrt 2 * r)
