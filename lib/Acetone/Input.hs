module Acetone.Input where
import Control.Concurrent.Chan

data KeyboardButton
  = KeyEscape | KeyEnter | KeySpace  -- &c.
  | KeyChar Char  -- ^ Use uppercase characters only!
  deriving (Show, Eq)

data KeyboardModifier
  = Shift | Control | Alt
  | Super | CapsLck | NumLck
  deriving (Show, Eq)

data MouseButton = LeftClick | MiddleClick | RightClick
                 deriving (Show, Eq)

data Button = Mouse MouseButton
            | Key [KeyboardModifier] KeyboardButton  -- ^ modifiers are in no particular order!ß
            deriving (Show, Eq)

data ButtonState = Pressed | Released | Repeating
                 deriving (Show, Eq)

data Event = Input ButtonState Button
           | Resize Int Int
           | Close  -- ^ Stop animating, close window.
           | RenderedFrame  -- ^ Very important event! 
                            --   It tells us to stop trying to read events off
                            --   the end of the event queue, which could cause
                            --   us to block indefinitely.
           -- &c.
           deriving (Show, Eq)

type EventQueue = Chan Acetone.Input.Event