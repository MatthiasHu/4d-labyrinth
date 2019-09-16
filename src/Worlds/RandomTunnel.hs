{-# LANGUAGE ScopedTypeVariables #-}

module Worlds.RandomTunnel
  ( randomTunnel
  ) where

import Control.Monad.Random
import qualified Data.Set as Set
import Linear hiding (translation)
import Control.Lens
import Data.Function (on)

import Constraints.Scalar
import Constraints.Vector
import Transformation
import SceneTO
import Objects.Cube
import Object
import Color
import Worlds.Tessellation as T


randomTunnel :: (MonadRandom m, SomeVector v, Ord p) =>
  Tessellation p v -> p -> m (SceneTO v Float, Transformation v Float)
randomTunnel t stop = do
  path <- randomPath t (origin t) stop
  let
    points =
      Set.fromList (concatMap (neighbours t) path)
      Set.\\ Set.fromList path
    rawTiles = map (tile t) (Set.toList points)
  tiles <- sequence
    [ (\c -> colorScene c s) <$> randomColor
    | s <- rawTiles
    ]
  return $ (SceneFork tiles, mempty)

randomPath :: (MonadRandom m, SomeVector v) =>
  Tessellation p v -> p -> p -> m [p]
randomPath t start stop = (start :) <$>
  if null choices
  then return []
  else do
    p <- uniform choices
    randomPath t p stop
  where
    choices = [ p | p <- neighbours t start, dist p stop < d ]
    d = dist start stop
    dist = distance `on` T.point t
