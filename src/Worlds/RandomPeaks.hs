module Worlds.RandomPeaks
  ( randomPeaks
  ) where

import Control.Monad.Random
import qualified Data.Map as Map
import Linear hiding (translation)

import Worlds.RandomColorBox
import Transformation
import Scene
import Object


randomPeaks :: (MonadRandom m, Floating a) =>
  Int -> m (Scene (Transformation a) (Object a), Transformation a)
randomPeaks n = do
  heights <- randomHeightMap n
  scene <- randomColorBox n $
    \(V3 x y z) -> (heights Map.! (x, y)) >= z
  return (scene, translation (V3 0 0 (fromIntegral (-n-1))))

randomHeightMap :: (MonadRandom m) =>
  Int -> m (Map.Map (Int, Int) Int)
randomHeightMap n = Map.fromList <$>
  forM xys (\xy -> (,) xy <$> randomHeight n)
  where
    xys = [ (x, y) | x <- [0..n], y <- [0..n] ]

randomHeight :: (MonadRandom m) => Int -> m Int
randomHeight n = weighted
  [ (i, weight i) | i <- [0..n] ]
  where
    weight i = fromIntegral (i+2) ^^(-2)
