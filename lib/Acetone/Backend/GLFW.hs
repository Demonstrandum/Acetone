module Acetone.Backend.GLFW (backend) where

import qualified Acetone
import qualified Acetone.Input as Input
import Acetone.Frontend

import Control.Exception
import Control.Monad (when, unless, forever)
import Control.Concurrent (writeChan, newChan)
import System.IO (stderr, hPutStrLn, hPutStr)

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=))

logger = hPutStrLn stderr . ("(:) " ++)

errorLog :: GLFW.Error -> String -> IO ()
errorLog code = logger . (("[err:" ++ show code ++ "] ") ++)

logError :: IO ()
logError = GLFW.getError >>= logger . logError'
    where logError' Nothing = "(no error set)"
          logError' (Just (code, message)) = "[err:" ++ show code ++ "] (" ++ message ++ ")"

backend :: Acetone.InternalState -> IO Acetone.InternalState
backend state = do
  action $ Acetone.rendererAction state
  where
    windowTitle = Acetone.windowTitle state
    initWindowSize = Acetone.initialWindowSize state
    clearBuffer = Acetone.beforeRedraw state
    swapBuffer = Acetone.afterRedraw state
    
    action :: RendererAction -> IO Acetone.InternalState
    action DoNothing = pure state
    action Init = initGLFW >> pure state
    action OpenWindow = do
      (win, eventQueue) <- createWindow windowTitle initWindowSize
      pure $ state { Acetone.beforeRedraw = beforeDisplay win
                   , Acetone.afterRedraw  = afterDisplay  win
                   , Acetone.eventQueue   = Just eventQueue
                   , Acetone.getMousePosition  = GLFW.getCursorPos win
                   , Acetone.getWindowPosition = GLFW.getWindowPos win
                   , Acetone.getWindowSize     = GLFW.getWindowSize win
                   }
    action ClearBuffer = clearBuffer >> pure state
    action ShowBuffer = swapBuffer >> pure state

initGLFW :: IO ()
initGLFW = do
  GLFW.getVersionString >>= logger . ("GLFW version: " ++) . show
  started <- GLFW.init
  logger $ "glfw init: " ++ show started
  if started
    then logger "ready" >> GLFW.setErrorCallback (Just errorLog)
    else logger "glfw failed to init" >> GLFW.terminate

createWindow :: String -> (Int, Int) -> IO (GLFW.Window, Input.EventQueue)
createWindow title (w, h) = do
  logger $ "opening window [" ++ title ++ "] with size (" ++ show w ++ ", " ++ show h ++ ")"
  GLFW.defaultWindowHints
  window <- GLFW.createWindow w h title Nothing Nothing
  case window of
    Nothing  -> logger "failed to create window" >> logError >> GLFW.terminate >> error "bail"
    Just win -> do
      -- Create event queue
      logger "created queue"
      eventQueue <- newChan
      -- Set callbacks to push to event queue
      GLFW.makeContextCurrent window
      GLFW.setWindowSizeCallback  win $ Just (resizeWindow eventQueue)
      GLFW.setWindowPosCallback   win $ Just (positionWindow eventQueue)
      GLFW.setKeyCallback         win $ Just (keyPressed eventQueue)
      GLFW.setCharCallback        win $ Just (typedChar eventQueue)
      GLFW.setMouseButtonCallback win $ Just (mouseButton eventQueue)
      GLFW.setCursorEnterCallback win $ Just (cursorEnter eventQueue)
      GLFW.setScrollCallback      win $ Just (scrollWheel eventQueue)
      GLFW.setWindowCloseCallback win $ Just (closeWindow eventQueue)
      -- Set some sane defaults
      GL.lineSmooth $= GL.Enabled
      GL.blend      $= GL.Enabled
      GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
      GL.lineWidth  $= 1.5
      GL.clearColor $= GL.Color4 0.67 0.194 0.255 1
      pure (win, eventQueue)
      --onDisplay win $ pure ()  -- get rid of this, let user choose
      --logger "display finished"
      --GLFW.destroyWindow win -- put in to InternalState shutdownCallback
  where closeWindow :: Input.EventQueue -> GLFW.WindowCloseCallback
        closeWindow queue window = do
            writeChan queue Input.Close
            GLFW.setWindowShouldClose window True
        resizeWindow :: Input.EventQueue -> GLFW.WindowSizeCallback
        resizeWindow queue window w h = do
          GL.viewport   $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))
          GL.matrixMode $= GL.Modelview 0 
          GL.loadIdentity
          GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
          writeChan queue $ Input.Resize w h
        positionWindow :: Input.EventQueue -> GLFW.WindowPosCallback
        positionWindow queue window x y = writeChan queue $ Input.Position x y
        getModifiers :: GLFW.ModifierKeys -> [Input.KeyboardModifier]
        getModifiers mods
          = [Input.Shift   | GLFW.modifierKeysShift    mods]
          ++ [Input.Control | GLFW.modifierKeysControl  mods] 
          ++ [Input.Alt     | GLFW.modifierKeysAlt      mods]
          ++ [Input.Super   | GLFW.modifierKeysSuper    mods]
          ++ [Input.CapsLck | GLFW.modifierKeysCapsLock mods]
          ++ [Input.NumLck  | GLFW.modifierKeysNumLock  mods]
        mouseButton :: Input.EventQueue -> GLFW.MouseButtonCallback
        mouseButton queue win button state mods = do
          let event = Input.Input (getState state) (Input.Mouse (getButton button)) (getModifiers mods)
          writeChan queue event
          where getState :: GLFW.MouseButtonState -> Input.ButtonState
                getState GLFW.MouseButtonState'Pressed = Input.Pressed
                getState GLFW.MouseButtonState'Released = Input.Released
                getButton :: GLFW.MouseButton -> Input.MouseButton
                getButton GLFW.MouseButton'1 = Input.LeftClick
                getButton GLFW.MouseButton'2 = Input.RightClick
                getButton GLFW.MouseButton'3 = Input.MiddleClick
                getButton _ = error "todo: handle more mouse buttons."
        cursorEnter :: Input.EventQueue -> GLFW.CursorEnterCallback
        cursorEnter queue _ GLFW.CursorState'InWindow    = writeChan queue (Input.CursorInside Input.EnteredWindow)
        cursorEnter queue _ GLFW.CursorState'NotInWindow = writeChan queue (Input.CursorInside Input.LeftWindow)
        scrollWheel :: Input.EventQueue -> GLFW.ScrollCallback
        scrollWheel queue _ x y = writeChan queue (Input.Scroll x y)
        typedChar :: Input.EventQueue -> GLFW.CharCallback
        typedChar queue win char = writeChan queue $ Input.Typed char
        keyPressed :: Input.EventQueue -> GLFW.KeyCallback
        keyPressed queue win key scancode depression modifiers = do
          -- discards scan-code.
          let event = Input.Input (getState depression) (Input.Key (getKey key)) (getModifiers modifiers)
          -- push to event queue
          writeChan queue event
          where getState :: GLFW.KeyState -> Input.ButtonState
                getState GLFW.KeyState'Pressed   = Input.Pressed
                getState GLFW.KeyState'Released  = Input.Released
                getState GLFW.KeyState'Repeating = Input.Repeating
                getKey :: GLFW.Key -> Input.KeyboardButton
                -- TODO: Finish this
                getKey GLFW.Key'Unknown    = Input.KeyUnknown scancode
                getKey GLFW.Key'Space	     = Input.KeySpace
                getKey GLFW.Key'Apostrophe = Input.KeyChar '\''
                getKey GLFW.Key'Comma	     = Input.KeyChar ','
                getKey GLFW.Key'Minus	     = Input.KeyChar '-'
                getKey GLFW.Key'Period	   = Input.KeyChar '.'
                getKey GLFW.Key'Slash	     = Input.KeyChar '/'
                getKey GLFW.Key'0	  = Input.KeyChar '0'
                getKey GLFW.Key'1	  = Input.KeyChar '1'
                getKey GLFW.Key'2	  = Input.KeyChar '2'
                getKey GLFW.Key'3	  = Input.KeyChar '3'
                getKey GLFW.Key'4	  = Input.KeyChar '4'
                getKey GLFW.Key'5	  = Input.KeyChar '5'
                getKey GLFW.Key'6	  = Input.KeyChar '6'
                getKey GLFW.Key'7	  = Input.KeyChar '7'
                getKey GLFW.Key'8	  = Input.KeyChar '8'
                getKey GLFW.Key'9	  = Input.KeyChar '9'
                getKey GLFW.Key'Semicolon	= Input.KeyChar ';'
                getKey GLFW.Key'Equal	    = Input.KeyChar '='
                getKey GLFW.Key'A	  = Input.KeyChar 'A'
                getKey GLFW.Key'B	  = Input.KeyChar 'B'
                getKey GLFW.Key'C	  = Input.KeyChar 'C'
                getKey GLFW.Key'D	  = Input.KeyChar 'D'
                getKey GLFW.Key'E	  = Input.KeyChar 'E'
                getKey GLFW.Key'F	  = Input.KeyChar 'F'
                getKey GLFW.Key'G	  = Input.KeyChar 'G'
                getKey GLFW.Key'H	  = Input.KeyChar 'H'
                getKey GLFW.Key'I	  = Input.KeyChar 'I'
                getKey GLFW.Key'J	  = Input.KeyChar 'J'
                getKey GLFW.Key'K	  = Input.KeyChar 'K'
                getKey GLFW.Key'L	  = Input.KeyChar 'L'
                getKey GLFW.Key'M	  = Input.KeyChar 'M'
                getKey GLFW.Key'N	  = Input.KeyChar 'N'
                getKey GLFW.Key'O	  = Input.KeyChar 'O'
                getKey GLFW.Key'P	  = Input.KeyChar 'P'
                getKey GLFW.Key'Q	  = Input.KeyChar 'Q'
                getKey GLFW.Key'R	  = Input.KeyChar 'R'
                getKey GLFW.Key'S	  = Input.KeyChar 'S'
                getKey GLFW.Key'T	  = Input.KeyChar 'T'
                getKey GLFW.Key'U	  = Input.KeyChar 'U'
                getKey GLFW.Key'V	  = Input.KeyChar 'V'
                getKey GLFW.Key'W	  = Input.KeyChar 'W'
                getKey GLFW.Key'X	  = Input.KeyChar 'X'
                getKey GLFW.Key'Y	  = Input.KeyChar 'Y'
                getKey GLFW.Key'Z	  = Input.KeyChar 'Z'
                getKey GLFW.Key'LeftBracket	 = Input.KeyChar '['
                getKey GLFW.Key'Backslash	   = Input.KeyChar '\\'
                getKey GLFW.Key'RightBracket = Input.KeyChar ']'
                getKey GLFW.Key'GraveAccent	 = Input.KeyChar '`'
                getKey GLFW.Key'World1	     = Input.Key161
                getKey GLFW.Key'World2	     = Input.Key162
                getKey GLFW.Key'Escape	     = Input.KeyEscape
                getKey GLFW.Key'Enter	    = Input.KeyEnter
                getKey GLFW.Key'Tab	      = Input.KeyTab
                getKey GLFW.Key'Backspace = Input.KeyBackspace
                getKey GLFW.Key'Insert	  = Input.KeyInsert
                getKey GLFW.Key'Delete	  = Input.KeyDelete
                getKey GLFW.Key'Right	    = Input.KeyRight
                getKey GLFW.Key'Left	    = Input.KeyLeft
                getKey GLFW.Key'Down	    = Input.KeyDown
                getKey GLFW.Key'Up	      = Input.KeyUp
                getKey GLFW.Key'PageUp	  = Input.KeyPgUp
                getKey GLFW.Key'PageDown	= Input.KeyPgDn
                getKey GLFW.Key'Home	    = Input.KeyHome
                getKey GLFW.Key'End	      = Input.KeyEnd
                getKey GLFW.Key'CapsLock	  = Input.KeyCapsLck
                getKey GLFW.Key'ScrollLock	= Input.KeySclLck
                getKey GLFW.Key'NumLock	    = Input.KeyNumLck
                getKey GLFW.Key'PrintScreen	= Input.KeyPrtScr
                getKey GLFW.Key'Pause	      = Input.KeyPause
                getKey GLFW.Key'F1	= Input.KeyF1
                getKey GLFW.Key'F2	= Input.KeyF2
                getKey GLFW.Key'F3	= Input.KeyF3
                getKey GLFW.Key'F4	= Input.KeyF4
                getKey GLFW.Key'F5	= Input.KeyF5
                getKey GLFW.Key'F6	= Input.KeyF6
                getKey GLFW.Key'F7	= Input.KeyF7
                getKey GLFW.Key'F8	= Input.KeyF8
                getKey GLFW.Key'F9	= Input.KeyF9
                getKey GLFW.Key'F10 = Input.KeyF10
                getKey GLFW.Key'F11 = Input.KeyF11
                getKey GLFW.Key'F12 = Input.KeyF12
                getKey GLFW.Key'F13 = Input.KeyF13
                getKey GLFW.Key'F14 = Input.KeyF14
                getKey GLFW.Key'F15 = Input.KeyF15
                getKey GLFW.Key'F16 = Input.KeyF16
                getKey GLFW.Key'F17 = Input.KeyF17
                getKey GLFW.Key'F18 = Input.KeyF18
                getKey GLFW.Key'F19 = Input.KeyF19
                getKey GLFW.Key'F20 = Input.KeyF20
                getKey GLFW.Key'F21 = Input.KeyF21
                getKey GLFW.Key'F22 = Input.KeyF22
                getKey GLFW.Key'F23 = Input.KeyF23
                getKey GLFW.Key'F24 = Input.KeyF24
                getKey GLFW.Key'F25 = Input.KeyF25
                getKey GLFW.Key'Pad0 = Input.KeyPad1
                getKey GLFW.Key'Pad1 = Input.KeyPad2
                getKey GLFW.Key'Pad2 = Input.KeyPad3
                getKey GLFW.Key'Pad3 = Input.KeyPad4
                getKey GLFW.Key'Pad4 = Input.KeyPad5
                getKey GLFW.Key'Pad5 = Input.KeyPad6
                getKey GLFW.Key'Pad6 = Input.KeyPad7
                getKey GLFW.Key'Pad7 = Input.KeyPad8
                getKey GLFW.Key'Pad8 = Input.KeyPad9
                getKey GLFW.Key'Pad9 = Input.KeyPad0
                getKey GLFW.Key'PadDecimal	  = Input.KeyPadDec
                getKey GLFW.Key'PadDivide	    = Input.KeyPadDiv
                getKey GLFW.Key'PadMultiply	  = Input.KeyPadMul
                getKey GLFW.Key'PadSubtract	  = Input.KeyPadSub
                getKey GLFW.Key'PadAdd	      = Input.KeyPadAdd
                getKey GLFW.Key'PadEnter  	  = Input.KeyPadEnter
                getKey GLFW.Key'PadEqual	    = Input.KeyPadEquals
                getKey GLFW.Key'LeftShift	    = Input.KeyLeftShift
                getKey GLFW.Key'LeftControl	  = Input.KeyLeftControl
                getKey GLFW.Key'LeftAlt	      = Input.KeyLeftAlt
                getKey GLFW.Key'LeftSuper	    = Input.KeyLeftSuper
                getKey GLFW.Key'RightShift	  = Input.KeyRightShift
                getKey GLFW.Key'RightControl  = Input.KeyRightControl
                getKey GLFW.Key'RightAlt	    = Input.KeyRightAlt
                getKey GLFW.Key'RightSuper	  = Input.KeyRightSuper
                getKey GLFW.Key'Menu          = Input.KeyMenu
                getKey _ = error "unhandled key, this is a bug."
                 
beforeDisplay :: GLFW.Window -> IO ()
beforeDisplay window = do
  GL.clear [GL.ColorBuffer]

afterDisplay :: GLFW.Window -> IO ()
afterDisplay window = do
  GLFW.swapBuffers window
  GLFW.pollEvents

  shouldClose <- GLFW.windowShouldClose window
  -- TODO: let user prevent closing the window on a close event,
  --       by letting them handle it explicitly in the event handler.
  when shouldClose $ GLFW.destroyWindow window >> GLFW.terminate
    
