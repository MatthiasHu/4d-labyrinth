module Blockworld
  ( Blockworld
  , emptyBlockworld
  , addBlock
  , removeBlock
  , blockworldScene
  , blockworldHomology
  ) where

import qualified Data.Map.Strict as Map

import Objects.Cube
import SceneTO
import CubicalHomology
import Transformation
import Color
import Constraints.Vector


type Blockworld v = Map.Map (v Int) Color

emptyBlockworld :: Blockworld v
emptyBlockworld = Map.empty

addBlock :: (SomeVector v) =>
  (v Int) -> Color -> Blockworld v -> Blockworld v
addBlock = Map.insert

removeBlock :: (SomeVector v) =>
  (v Int) -> Blockworld v -> Blockworld v
removeBlock = Map.delete


blockworldScene :: (SomeVector v, Floating a) =>
  Blockworld v -> SceneTO v a
blockworldScene world = SceneFork
  [ makeCube pos color
  | (pos, color) <- Map.assocs world ]
  where
    makeCube v c = Transformed (translation $ fmap fromIntegral v)
      . SceneObject $ cube 0.5 c

blockworldHomology :: (SomeVector v) => Blockworld v -> Homology
blockworldHomology = cubicalHomology . Map.keys
