{-# LANGUAGE ScopedTypeVariables #-}

module Objects.Cube
  ( cube
  , cubeWireframe
  , cubeWithWireframe
  ) where

import Linear
import Linear.Affine
import Control.Lens
import Data.List

import Object
import Wireframe
import Color
import Geometry.Hyperplane
import Constraints.Vector


cube :: forall v a. (SomeVector v, Floating a) =>
  a -> Object v a
cube radius =
  simpleObject (sqrt dim * radius) (cubeFaces radius)
  & objectInnerRadius .~ (sqrt 2 * radius)
  where
    dim = sum (pure 1 :: v a)

cubeFaces :: (SomeVector v, Floating a) => a -> [Hyperplane v a]
cubeFaces radius = do
  v <- basis
  sign <- [-1, 1]
  return $ Hyperplane (sign *^ v) radius

cubeWireframe :: forall v a. (SomeVector v, Num a) =>
  a -> Wireframe v a
cubeWireframe radius = Wireframe $ do
  v <- basis :: [Point v Int]
  w <- sum' <$> subsequences (delete v basis)
  return $ over both (fmap stretch) (w, v^+^w)
  where
    sum' = foldl' (^+^) zero
    stretch x = fromIntegral (2*x - 1) * radius

cubeWithWireframe :: (SomeVector v, Floating a) =>
  a -> Object v a
cubeWithWireframe radius =
  cube radius & objectWireframe .~ Just (cubeWireframe radius)
