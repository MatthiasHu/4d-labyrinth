{-# LANGUAGE Rank2Types #-}

module Worlds.RandomTunnel
  ( randomTunnel
  ) where

import Control.Monad.Random
import qualified Data.Set as Set
import Linear hiding (translation)
import Control.Lens

import Worlds.RandomColorBox
import Transformation
import Scene
import Object


randomTunnel :: (MonadRandom m, Floating a) =>
  Int -> m (Scene (Transformation a) (Object a), Transformation a)
randomTunnel n = do
  path <- randomPath (pure (n-1)) (pure 1)
  scene <- randomColorBox n (`Set.notMember` (Set.fromList path))
  return (scene, translation (V3 (-1) (-1) (-1)))

-- A random monotonous path connecting two points.
randomPath :: (MonadRandom m) => V3 Int -> V3 Int -> m [V3 Int]
randomPath a b = if null dirs then return [a]
  else do
    d <- uniform dirs
    let a' = if a ^. lens d > b ^. lens d
             then a & lens d -~ 1
             else a & lens d +~ 1
    rest <- randomPath a' b
    return (a:rest)
  where
    -- This ugly list of Ints is here
    -- because Lenses don't like to be in lists very much.
    -- (impredicative polymorphism errors...)
    dirs :: [Int]
    dirs = filter (\d -> a^.lens d /= b^.lens d) [1, 2, 3]
    lens 1 = _x
    lens 2 = _y
    lens 3 = _z
