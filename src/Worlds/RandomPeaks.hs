{-# LANGUAGE Rank2Types #-}

module Worlds.RandomPeaks
  ( randomPeaks
  ) where

import Control.Monad.Random
import qualified Data.Map as Map
import Linear hiding (translation)
import Control.Lens

import Constraints.Scalar
import Worlds.RandomColorBox
import Transformation
import SceneTO
import Constraints.Vector
--import Objects.Tree


randomPeaks ::
  (MonadRandom m, SomeVector v, SomeVector v', SomeScalar a) =>
  (forall b. Lens' (v b) (v' b)) -> (forall b. Lens' (v b) b)
  -> Int -> m (SceneTO v a, Transformation v a)
randomPeaks horizontal vertical n = do
  heights <- randomHeightMap n
  boxes <- randomColorBox n $
    \pos -> (heights Map.! (pos^.horizontal)) >= (pos^.vertical)
{-  let
    translated x y z =
      Transformed . translation . (^+^ V3 0 0 0.5) . fmap fromIntegral
        $ V3 x y z
    trees = SceneFork
      [ translated x y (heights Map.! (x, y)) tree
      | (x, y) <- peaks n heights ]-}
  return ( boxes --SceneFork [boxes, trees]
         , translation (zero & vertical .~ fromIntegral (-n-1)) )

randomHeightMap :: (MonadRandom m, SomeVector v) =>
  Int -> m (Map.Map (v Int) Int)
randomHeightMap n = Map.fromList <$>
  forM xs (\x -> (,) x <$> randomHeight n)
  where
    xs = sequenceA (pure [0..n])

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
