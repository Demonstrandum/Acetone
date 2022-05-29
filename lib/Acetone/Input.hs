module Acetone.Input where
import Control.Concurrent.Chan

data KeyboardButton
  = KeyEscape | KeyEnter | KeySpace | KeyTab
  | KeyBackspace | KeyInsert | KeyDelete
  | KeyUp | KeyLeft | KeyDown | KeyRight
  | KeyPgUp | KeyPgDn | KeyHome | KeyEnd 
  | KeyCapsLck | KeySclLck | KeyNumLck | KeyPrtScr
  | KeyPause
  | KeyF1 | KeyF2 | KeyF3 | KeyF4 | KeyF5
  | KeyF6 | KeyF7 | KeyF8 | KeyF9 | KeyF10
  | KeyF11 | KeyF12 | KeyF13 | KeyF14 | KeyF15
  | KeyF16 | KeyF17 | KeyF18 | KeyF19 | KeyF20
  | KeyF21 | KeyF22 | KeyF23 | KeyF24 | KeyF25
  | KeyPad0 | KeyPad1 | KeyPad2 | KeyPad3
  | KeyPad4 | KeyPad5 | KeyPad6 | KeyPad7
  | KeyPad8 | KeyPad9 | KeyPadDec
  | KeyPadDiv | KeyPadMul | KeyPadAdd
  | KeyPadSub | KeyPadEnter | KeyPadEquals
  | KeyLeftShift  | KeyLeftControl  | KeyLeftAlt  | KeyLeftSuper
  | KeyRightShift | KeyRightControl | KeyRightAlt | KeyRightSuper
  | KeyMenu | Key161 | Key162
  | KeyChar Char  -- ^ Use uppercase alphabetical characters only! 
                  --   Keys are referred to as their non-modifier variants except for the alphabetic keys.
                  --   (e.g. the key's value without holding shift, '=' not '+')
                  --   Only visible characters count, whitespace is separate.
  | KeyUnknown Int  -- ^ A scancode is provided in case the key is not listed here.
  deriving (Show, Eq)

data KeyboardModifier  -- | Modifiers are in no particular order!
  = Shift | Control | Alt
  | Super | CapsLck | NumLck
  deriving (Show, Eq)

data MouseButton = LeftClick | MiddleClick | RightClick
                 deriving (Show, Eq)

data Button = Mouse MouseButton
            | Key KeyboardButton  
            deriving (Show, Eq)

data ButtonState = Pressed | Released | Repeating
                 deriving (Show, Eq)

-- | UCS-4 (unicode) code point.
type Codepoint = Char 

data CursorInWindow = EnteredWindow | LeftWindow
                    deriving (Show, Eq)

data Event = Input ButtonState Button [KeyboardModifier]  -- ^ When a specific mouse or keyboard button is pressed, released or repeated.
           | Typed Codepoint  -- ^ When a full unicode character is typed.
           | Position Int Int
           | Resize Int Int
           | CursorInside CursorInWindow
           | MousePosition Double Double  -- ^ Normalised to window.
           | Scroll Double Double  -- ^ Scroll offset in x and y directions.
           | Close  -- ^ Stop animating, close window.
           | RenderedFrame  -- ^ Very important event! 
                            --   It tells us to stop trying to read events off
                            --   the end of the event queue, which could cause
                            --   us to block indefinitely.
           -- &c.
           deriving (Show, Eq)

type EventQueue = Chan Acetone.Input.Event