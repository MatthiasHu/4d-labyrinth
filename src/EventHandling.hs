module EventHandling
  ( handleEvent
  ) where

import SDL
import Control.Lens
import Data.Monoid
import Data.Int

import State
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
handleEvent (WindowSizeChangedEvent _) =
  handleWindowSizeChanged
handleEvent _ = return

handleKeyPressed :: Keysym -> State v -> IO (State v)
handleKeyPressed (Keysym _ KeycodeEscape _) _ = quitAndExit
handleKeyPressed _ s = return s

handleMouseMotion :: (SomeVector v) =>
  V2 Int32 -> State v -> IO (State v)
handleMouseMotion (V2 dx dy) s = return $ s & over eye
  ((s ^. rotationMethod) ( fromIntegral dx    * sensitivity
                         , fromIntegral (-dy) * sensitivity ) <>)
  where
    sensitivity = 0.005

handleWindowSizeChanged :: State v -> IO (State v)
handleWindowSizeChanged s = do
  setViewport (s ^. window)
  return s
