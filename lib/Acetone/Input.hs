module Acetone.Input where

import Prelude.Unicode
import Control.Concurrent.Chan
import Data.Char (toUpper, isSpace)
import Data.List (dropWhileEnd)
-- Type-level literal verification of KeyChar.
import Data.Typeable
import Data.Data
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote as THQ

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
  | KeyChar Char  -- ^ Use only _uppercase_ alphabetical characters,
                  --   other keys are referred to as their non-modifier variants (except for the alphabetic keys)
                  --   (i.e. the key's value without holding shift, '=' not '+')
                  --   Only visible characters count, whitespace is separate.
  | KeyUnknown Int  -- ^ A scancode is provided in case the key is not listed here.
  deriving (Show, Eq, Data, Typeable)

spaceKeys = ["Space", " "]
escapeKeys = ["Escape", "Esc"]
equalKeys = ["+", "=", "Plus", "Equal", "Equals"]
hyphenKeys = ["-", "_"]

-- TODO: Finish all possible keyboard keys on a standard US keyboard.
makeKeyChar :: MonadFail m => String -> m KeyboardButton
makeKeyChar "" = fail "No character to match."
makeKeyChar key
  | key ∈ spaceKeys  = pure KeySpace
  | key ∈ escapeKeys = pure KeyEscape
  | key ∈ equalKeys  = pure $ KeyChar '='
  -- TODO: Finish rest of special symbol keys.
  | key == [head key] =  pure . KeyChar . toUpper $ head key
  | ' ' ∈ [head key, last key] = makeKeyChar . trim $ key
  | otherwise = fail $ "No corresponding key: `" ++ key ++ "'."
  where trim = dropWhileEnd isSpace . dropWhile isSpace

makeKeyCharExp :: String -> TH.Q TH.Exp
makeKeyCharExp s = makeKeyChar s >>= dataToExpQ (const Nothing)

makeKeyCharPat :: String -> TH.Q TH.Pat
makeKeyCharPat s = makeKeyChar s >>= dataToPatQ (const Nothing)

-- | Construct a `(KeyChar Char)::KeyboardButton`  with less
-- chance of making a mistake with whether Shift or Alt was press &c.
-- i.e. accepts multiple variants of the same key.
-- That is: `[char|Space]` == `[char| |]` == `KeySpace`;
-- `[char|Esc|]` == `[char|Escape|]` == `KeyEscape`;
-- `[char|q|]` == `[char|Q|]` == `KeyChar 'Q'`.
char :: THQ.QuasiQuoter
char = QuasiQuoter { quoteExp = makeKeyCharExp
                   , quotePat = makeKeyCharPat
                   , quoteDec = error "invalid use"
                   , quoteType = error "invalid use"
                   }

-- | Construct a `(Input Pressed (Key KeyboardButton) [KeyBoardModifier])::Event`
-- by parsing common keyboard combination notation.
-- Example: `[press| Ctrl + Shift + D |]`
--       == `[press|control+shift+d|]`
--       == `Input Pressed (Key KeyChar 'D') [Shift, Control]`.
press :: THQ.QuasiQuoter
press = error "not implemented"

-- | Construct a `(Input Released (Key KeyboardButton) [KeyBoardModifier])::Event`
-- by parsing common keyboard combination notation.
-- Example: `[release| Super + Alt + = |]`
--       == `[release|command+option+plus|]`
--       == `Input Pressed (Key KeyChar '=') [Alt, Super]`.
release :: THQ.QuasiQuoter
release = error "not implemented"

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

-- | All types of events that can be handled.
-- | Use `Input` to handle button presses, keyboard and mouse.
-- | Use `Typed` to handle text input. Fires each time a full unicode character is typed.
-- | `Resize` event is fired each time the drawing surface is resized (if possible).
-- | `
data Event = Input ButtonState Button [KeyboardModifier]  -- ^ When a specific mouse or keyboard button is pressed, released or repeated.
           | Typed Codepoint  -- ^ When a full unicode character is typed.
           | Position Int Int  -- ^ Surface position on screen, pixels.
           | Resize Int Int  -- ^ Surafce is resized, pixel dimensions.
           | CursorInside CursorInWindow  -- ^ Cursor has entered or left the window.
           | MousePosition Double Double  -- ^ Position of cursor, normalised to window.
           | Scroll Double Double  -- ^ Scroll offset in x and y directions.
           | Close  -- ^ Stop animating, close window/file/&c.
           | RenderedFrame  -- ^ Very important event!
                            --   It tells us to stop trying to read events off
                            --   the end of the event queue, which could cause
                            --   us to block indefinitely.
           -- &c.
           deriving (Show, Eq)

type EventQueue = Chan Acetone.Input.Event
