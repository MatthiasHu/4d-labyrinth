module Worlds.RandomPeaks
  ( randomPeaks
  ) where

import Control.Monad.Random
import qualified Data.Map as Map
import Linear hiding (translation)

import Worlds.RandomColorBox
import Transformation
import SceneTO
import Objects.Tree


randomPeaks :: (MonadRandom m, Floating a) =>
  Int -> m (SceneTO a, Transformation a)
randomPeaks n = do
  heights <- randomHeightMap n
  boxes <- randomColorBox n $
    \(V3 x y z) -> (heights Map.! (x, y)) >= z
  let
    translated x y z = Transformed . translation . (^+^ V3 0 0 0.5) . fmap fromIntegral $ V3 x y z
    trees = SceneFork [ translated x y (heights Map.! (x, y)) tree
                        | (x, y) <- peaks n heights ]
  return ( SceneFork [boxes, trees]
         , translation (V3 0 0 (fromIntegral (-n-1))) )

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

peaks :: Int -> Map.Map (Int, Int) Int -> [(Int, Int)]
peaks n m = filter isPeak [(x, y) | x<-[1..(n-1)], y<-[1..(n-1)]]
  where
    isPeak (x, y) = let z = m Map.! (x, y)
      in all ((<z) . (m Map.!))
           [(x, y+1), (x, y-1), (x+1, y), (x-1, y)]
