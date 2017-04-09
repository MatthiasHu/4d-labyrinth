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


handleEvent :: EventPayload -> State -> IO State
handleEvent (WindowClosedEvent _) = const quitAndExit
handleEvent (KeyboardEvent (KeyboardEventData _ Pressed _ keysym)) =
  handleKeyPressed keysym
handleEvent (MouseMotionEvent (MouseMotionEventData _ _ _ _ d)) =
  handleMouseMotion d
handleEvent (WindowSizeChangedEvent _) =
  handleWindowSizeChanged
handleEvent _ = return

handleKeyPressed :: Keysym -> State -> IO State
handleKeyPressed (Keysym _ KeycodeEscape _) _ = quitAndExit
handleKeyPressed _ s = return s

handleMouseMotion :: V2 Int32 -> State -> IO State
handleMouseMotion (V2 dx dy) = return . over eye
  ((   rotation _xz (fromIntegral (-dx) * sensitivity)
    <> rotation _yz (fromIntegral dy    * sensitivity) ) <>)
  where
    sensitivity = 0.005

handleWindowSizeChanged :: State -> IO State
handleWindowSizeChanged s = do
  setViewport (s ^. window)
  return s
