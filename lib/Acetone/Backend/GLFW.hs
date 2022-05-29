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
    windowSize = Acetone.windowSize state
    clearBuffer = Acetone.beforeRedraw state
    swapBuffer = Acetone.afterRedraw state
    
    action :: RendererAction -> IO Acetone.InternalState
    action DoNothing = pure state
    action Init = initGLFW >> pure state
    action OpenWindow = do
      (win, eventQueue) <- createWindow windowTitle windowSize
      pure $ state { Acetone.beforeRedraw = beforeDisplay win
                   , Acetone.afterRedraw  = afterDisplay  win
                   , Acetone.eventQueue   = Just eventQueue
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
      GLFW.setKeyCallback         win $ Just (keyPressed eventQueue)
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
          writeChan queue (Input.Resize w h)
        keyPressed :: Input.EventQueue -> GLFW.KeyCallback
        keyPressed queue win key _ depression modifiers = do
          -- discards scan-code.
          let event = Input.Input
                      (getState depression) $
                      Input.Key (getModifiers modifiers) (getKey key)
          -- push to event queue
          logger "writing..."
          writeChan queue event
          logger "wrote to queue"
          where getState :: GLFW.KeyState -> Input.ButtonState
                getState GLFW.KeyState'Pressed   = Input.Pressed
                getState GLFW.KeyState'Released  = Input.Released
                getState GLFW.KeyState'Repeating = Input.Repeating
                getModifiers :: GLFW.ModifierKeys -> [Input.KeyboardModifier]
                getModifiers mods
                  = [Input.Shift   | GLFW.modifierKeysShift    mods]
                 ++ [Input.Control | GLFW.modifierKeysControl  mods] 
                 ++ [Input.Alt     | GLFW.modifierKeysAlt      mods]
                 ++ [Input.Super   | GLFW.modifierKeysSuper    mods]
                 ++ [Input.CapsLck | GLFW.modifierKeysCapsLock mods]
                 ++ [Input.NumLck  | GLFW.modifierKeysNumLock  mods]
                getKey :: GLFW.Key -> Input.KeyboardButton
                -- TODO: Finish this
                getKey GLFW.Key'Q = Input.KeyChar 'Q'
                getKey GLFW.Key'Escape = Input.KeyEscape
                getKey GLFW.Key'Space = Input.KeySpace
                getKey GLFW.Key'Enter = Input.KeyEnter
                getKey _ = error "button not supported"
                 
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
    
