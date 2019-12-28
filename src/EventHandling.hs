module EventHandling
  ( handleEvent
  ) where

import SDL
import Control.Lens
import Data.Monoid
import Data.Int

import State
import RotationMethods
import Transformation
import Setup
import Constraints.Vector


handleEvent :: (SomeVector v) =>
  EventPayload -> State v -> IO (State v)
handleEvent (WindowClosedEvent _) = const quitAndExit
handleEvent (KeyboardEvent (KeyboardEventData _ Pressed _ keysym)) =
  handleKeyPressed keysym
handleEvent (MouseMotionEvent (MouseMotionEventData _ _ _ _ d)) =
  handleMouseMotion d
handleEvent (MouseWheelEvent (MouseWheelEventData _ _ d _)) =
  handleMouseWheel d
handleEvent (WindowSizeChangedEvent _) =
  handleWindowSizeChanged
handleEvent _ = return

handleKeyPressed :: Keysym -> State v -> IO (State v)
handleKeyPressed (Keysym _ KeycodeEscape _) _ = quitAndExit
handleKeyPressed _ s = return s

handleMouseMotion :: (SomeVector v) =>
  V2 Int32 -> State v -> IO (State v)
handleMouseMotion (V2 dx dy) = return . rotationInput
  ( fromIntegral dx    * sensitivity
  , fromIntegral (-dy) * sensitivity
  , 0
  )
  where
    sensitivity = 0.005

handleMouseWheel :: (SomeVector v) =>
  V2 Int32 -> State v -> IO (State v)
handleMouseWheel (V2 _ dy) = return . rotationInput
  (0, 0, fromIntegral dy * sensitivity)
  where
    sensitivity = 0.05

rotationInput :: (SomeVector v) =>
  RotationInput Scalar -> State v -> State v
rotationInput input s = s & over eye
  ((s ^. rotationMethod . inputRotation) input <>)

handleWindowSizeChanged :: State v -> IO (State v)
handleWindowSizeChanged s = do
  setViewport (s ^. window)
  return s
