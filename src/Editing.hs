module Editing
  ( addBlock
  , addBlockHere
  , removeBlock
  , removeBlockHere
  ) where

import Linear
import Linear.Affine
import Control.Lens hiding (transform)

import qualified Blockworld as BW
import Color
import State
import Transformation
import Constraints.Vector


addBlockHere :: (SomeVector v, R3 v) => Color -> State v -> State v
addBlockHere color s = s & addBlock (editingSpot (s ^. eye)) color

removeBlockHere :: (SomeVector v, R3 v) => State v -> State v
removeBlockHere s = s & removeBlock (editingSpot (s ^. eye))

-- which grid position to edit given this eye transformation
editingSpot :: (SomeVector v, R3 v) => Transformation v Scalar -> v Int
editingSpot eye = unP $ round <$>
  transform (invert eye) (zero & _z .~ (-1.5))
  where
    unP (P v) = v

addBlock :: (SomeVector v) => v Int -> Color -> State v -> State v
addBlock spot color = updateBlockworld (BW.addBlock spot color)

removeBlock :: (SomeVector v) => v Int -> State v -> State v
removeBlock spot = updateBlockworld (BW.removeBlock spot)

updateBlockworld :: (SomeVector v) =>
  (BW.Blockworld v -> BW.Blockworld v) -> State v -> State v
updateBlockworld f s = s
  & blockworld .~ bw
  & scene .~ BW.blockworldScene bw
  where
    bw = f (s ^. blockworld)
