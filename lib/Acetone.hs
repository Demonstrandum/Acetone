{-|
    Module      : Acetone
    Description : Simple 2D graphics library with support for multiple backends.
    Licence     : GPL-3
    Maintainer  : Samuel <samuel@knutsen.co>
    Portability : Linux (X11/Wayland), macOS (Quartz), Windows (DWM)
|-}

module Acetone ( module Acetone
               , module Acetone.Utils
               , module Data.Default
               , liftIO
               ) where
-- Export all contents, and re-export some imports.

import Data.Default
import System.IO (hPutStr, stderr)
import Control.Arrow ((***))
import Control.Monad.State
import Data.Functor ((<&>))

import Acetone.Utils
import qualified Acetone.Input as Input
import qualified Acetone.Frontend as Frontend
import qualified Acetone.Shapes as Shapes
import Acetone.Shapes (Distance)

import Control.Concurrent (writeChan, readChan)

-- | Used to represent pixel sizes internally.
type Pixels = Int
-- | Elapsed time (time deltas) in milliseconds
type Elapsed = Double
-- | A duration of time from arbitrary starting point, in milliseconds
type Duration = Double
-- | Refresh rate, integer number of frames displayed per second
type FPS = Int
-- | An event handler produces a GraphicsMonad given an event.
type EventHandler a = Input.Event -> GraphicsMonad a ()

-- | Basic internal state for window management and such.
data InternalState = InternalState
    { lastIO :: IO ()  -- ^ Context of last IO operations.  Useful to recontextualise a call to `liftIO`.
    , windowTitle :: String
    , initialWindowSize :: (Pixels, Pixels)
    , initialWindowPosition :: (Pixels, Pixels)
    , shouldCloseWindow :: Bool
    , rendererAction :: Frontend.RendererAction
    , backendUpdate :: InternalState -> IO InternalState  -- ^ Only call with an intention (renderer action).
    , beforeRedraw :: IO ()
    ,  afterRedraw :: IO ()
    , getMousePosition :: IO (Double, Double)
    , getWindowPosition :: IO (Pixels, Pixels)
    , getWindowSize :: IO (Pixels, Pixels)
    , getFrameSize  :: IO (Pixels, Pixels)
    , closeWindow :: IO ()
    , eventQueue :: Maybe Input.EventQueue  -- ^ Channel of events.
    , picture :: Shapes.Picture
    , refreshCallback :: IO () -> IO ()  -- ^ Takes in the monad which draws our scene, such that it may be redrawn when a refresh is needed.
    }

instance Default InternalState where
    def = InternalState (pure ()) "" (0, 0) (0, 0) False Frontend.DoNothing
                        (\i -> hPutStr stderr "error: no backend installed!\n" >> pure i)
                        (hPutStr stderr "error: no buffer/window exists yet\n")
                        (hPutStr stderr "error: no buffer/window exists yet\n")
                        (pure (0, 0)) (pure (0, 0)) (pure (0, 0)) (pure (0, 0))
                        (pure ()) Nothing mempty (const $ pure ())

-- | Generic state and IO monad for some given user state structure.
-- @state@ is the generic type for the user's application's state.
-- @value@ is the value that monad wraps.
-- e.g.
-- >    newtype MyGameState = MyGameState { bg :: Color, fg :: Color }
-- >    type GameIO = `GraphicsMonad` MyGameState  -- GameIO is a monad.
-- >
-- >    runGame :: GameIO ()
-- >    runGame = `setState` $ MyGameState black white  -- &c.
-- >
-- >    main :: IO ()
-- >    main = `setupGraphics` runGame
newtype GraphicsMonad state value
    = GraphicsMonad (StateT (InternalState, state) IO value)
    deriving (Functor, Applicative, Monad)

getStateT :: GraphicsMonad a b -> StateT (InternalState, a) IO b
getStateT gm = stateT where (GraphicsMonad stateT) = gm

instance MonadIO (GraphicsMonad state) where
    liftIO action = GraphicsMonad (lift action)

instance MonadState (InternalState, state) (GraphicsMonad state) where
    get = GraphicsMonad get
    put = GraphicsMonad . put

pullState :: GraphicsMonad state (InternalState, state)
pullState = get
pushState :: (InternalState, state) -> GraphicsMonad state ()
pushState = put

getInternalState :: GraphicsMonad a InternalState
getInternalState = fst <$> pullState

setInternalState :: InternalState -> GraphicsMonad a ()
setInternalState internal = pullState >>= pushState . ((internal, ) <$> snd)

-- | Pulls user-facing state.
getState :: GraphicsMonad state state
getState = snd <$> pullState

-- | Updates the user-facing state.
setState :: a -> GraphicsMonad a ()
setState userState =  pullState >>= pushState . (, userState) . fst

windowSize :: GraphicsMonad a (Int, Int)
windowSize = getInternalState >>= liftIO . getWindowSize

normalisePixel :: Real n => n -> GraphicsMonad a Distance
normalisePixel x = do
  winSize <- windowSize
  let (w, h) = join (***) fromIntegral winSize
  pure $ if w > h
    then realToFrac x / h
    else realToFrac x / w

normalizePixels :: Real n => (n, n) -> GraphicsMonad a (Distance, Distance)
normalizePixels (pixelsLeft, pixelsDown) = (,) <$> normalisePixel pixelsLeft <*> normalisePixel pixelsDown

-- | Get a distance from the proportion of the height (1.0 at the bottom, 0.0 at the top)
ofHeight :: Double -> GraphicsMonad a Distance
ofHeight = (windowSize >>= normalisePixel . snd <&>) . (*)

-- | Get a distance from the proportion of the width (1.0 at the right, 0.0 at the left)
ofWidth :: Double -> GraphicsMonad a Distance
ofWidth = (windowSize >>= normalisePixel . fst <&>) . (*)

-- | Gets the mouse position in normalised units.
mousePosition :: GraphicsMonad a (Distance, Distance)
mousePosition = (getInternalState >>= (liftIO . getMousePosition)) >>= normalizePixels

-- | Open the drawing surface in the current context.
-- This is either a window or file, (&c.) depending on the backend chosen.
-- A title, size, and position (where applicable) must be suppplied.
-- This variant of `openSurface` requires state have a Default instance.
openSurface :: Default state => String -> (Int, Int) -> (Int, Int) -> GraphicsMonad state ()
openSurface title size pos = do
  i <- getInternalState
  let internal = i {
        windowTitle           = title
      , initialWindowSize     = size
      , initialWindowPosition = pos
      , rendererAction        = Frontend.OpenSurface
      }
  setInternalState internal
  backendCallback

backendCallback :: GraphicsMonad state ()
backendCallback = do
  oldState <- getInternalState
  -- Funky stuff.  `lastIO` contains the context of the last IO operation.
  -- Using `liftIO` looses the context of the IO operations each time.
  -- We recontextualise the operations from the `backendUpdate` callback with the
  -- last IO actions, then update the `lastIO` field with `pure ()`, which just
  -- recaptures the IO state up until this point.
  newState <- liftIO $ lastIO oldState
                    >> backendUpdate oldState oldState
                   >>= \new -> pure $ new { lastIO = pure () }
  setInternalState newState

-- | Send a renderer action to the backend.
-- The state is updated with the action, and the backend callback is
-- immediately run.
pulseAction :: Frontend.RendererAction -> GraphicsMonad state ()
pulseAction action = do
  old <- getInternalState
  let new = old { rendererAction = action }
  setInternalState new
  backendCallback

installBackend :: (InternalState -> IO InternalState) -> GraphicsMonad state ()
installBackend backend = do oldInternal <- getInternalState
                            let newInternal = oldInternal {
                                  backendUpdate = backend
                                , rendererAction = Frontend.Init
                                }
                            setInternalState newInternal
                            backendCallback  -- Send `Init` signal as soon as backend is installed.

-- TODO: Better name?  Just allows `main :: IO ()` to use `Context ()` monad.
--       graphicsMain; initGraphics; acetoneMain; sinkIO; runGraphics; runRenderer;
setupGraphics :: Default state => GraphicsMonad state () -> IO ()
setupGraphics setupMonad = evalStateT (getStateT setupMonad) def

-- | Draw a picture to the screen.
draw :: Shapes.Picture -> GraphicsMonad a ()
draw pic = do
  old <- getInternalState
  setInternalState $ old { picture = pic }
  pulseAction Frontend.DrawPicture
  new <- getInternalState
  setInternalState $ new { picture = mempty }

-- | Perform drawing of the frame with the graphics monad given an elapsed time.
-- First argument provides the number of frames to aim to display each second,
-- second is the callback to be performed each frame. Example:
-- >   `animationFrame` 24 (\ε -> pollEvents >>= mapM_ eventHandler >> buildPicture)
-- Use `simulationLoop` if you want to update a state every frame in a pure manner.
-- Otherwise, state can be managed by `animationFrame` by using the monadic
-- `getState` and `setState` function, but this is less _functional_-style.
animationFrame :: FPS -> (Elapsed -> GraphicsMonad a Shapes.Picture) -> GraphicsMonad a ()
animationFrame 0 _ = pure ()
animationFrame fps callback = executeFrequency (fromIntegral fps) () renderPicture
  where renderPicture _ elapsed = do
          pulseAction Frontend.ClearBuffer
          sendEvent Input.RenderedFrame  -- (!) must indicate end of previous frame rendering.
          scene <- callback elapsed
          draw scene
          pulseAction Frontend.ShowBuffer

-- | Same as `animationFrame` but with a remebered simualtion-state, which gets updated by the callback.
-- The callback is supplied the last state and the time elapsed since the last iteration,
-- and must return a tuple of the updated state and the scene to render. Example:
-- >   `simulationLoop` 60 (\oldState ε -> pollEvents >>= handleEvents (updateState oldState ε) >>= \newState -> (newSate, buildScene newState))
simulationLoop :: Default a => FPS -> (a -> Elapsed -> GraphicsMonad a (a, Shapes.Picture)) -> GraphicsMonad a ()
simulationLoop 0 _ = pure ()
simulationLoop fps callback = executeFrequency (fromIntegral fps) def renderPicture
  where renderPicture lastState elapsed = do
          pulseAction Frontend.ClearBuffer
          sendEvent Input.RenderedFrame
          (newState, scene) <- callback lastState elapsed
          draw scene
          pulseAction Frontend.ShowBuffer
          pure newState

-- | Execute callback at given frequency (Hz).
-- Uses a combination of innaccurate `sleep`/`threadDelay` and
-- a spin-lock in order to give the CPU some rest while maintaining
-- a more precise inter-frame time to stabalize the FPS.
executeFrequency :: Double -> b -> (b -> Elapsed -> GraphicsMonad a b) -> GraphicsMonad a ()
executeFrequency 0 _ _ = pure ()
executeFrequency hz initialState callback = now >>= executeFrequency' initialState . subtract frameTime
  where now = liftIO monotonicMillis
        frameTime = 1000.0 / hz :: Elapsed  -- target frame time in milliseconds
        executeFrequency' lastState lastTick = do
          tick <- now
          newState <- callback lastState (tick - lastTick)
          tock <- now

          let delta = tock - tick
          let sleepyTime = frameTime - delta
          when (sleepyTime > 0) $
            liftIO $ preciseDelay sleepyTime

          stop <- shouldCloseWindow <$> getInternalState
          if stop then pulseAction Frontend.Terminate
                  else executeFrequency' newState tick

-- | Loads events from the event queue to a list of events such that you may
-- perform actions on each of these events.  These would usually be passed
-- to an event handler, e.g.
-- >     `pollEvents` >>= mapM eventHandler >>= drawScene . last
-- >     -- where we have
-- >     eventHandler :: Event -> `GraphicsMonad` () GameState
-- >     eventHandler (Input ... ) = ...
-- >     drawScene :: GameState -> `GraphicsMonad` () ()
-- >     drawScene (GameState ...) = draw (...)
-- Of course, you may do something completely different, since the above
-- is a bit naïve.
pollEvents :: GraphicsMonad a [Input.Event]
pollEvents = do
  queue <- eventQueue <$> getInternalState
  case queue of
    Just events -> (:) <$> (uncurry Input.MousePosition <$> mousePosition) <*> readInputEvents events
    Nothing -> error "error: polled event queue before initialisation"
  where readInputEvents :: Input.EventQueue -> GraphicsMonad a [Input.Event]
        readInputEvents events = liftIO (readChan events) >>= buildEvents
          where buildEvents :: Input.Event -> GraphicsMonad a [Input.Event]
                buildEvents event
                  | event == Input.RenderedFrame = pure [event]
                  | event == Input.Close = goingToCloseWindow >> pure [event]
                  | otherwise = (event :) <$> readInputEvents events

sendEvent :: Input.Event -> GraphicsMonad a ()
sendEvent event = sendEvent' . eventQueue =<< getInternalState
  where sendEvent' :: Maybe Input.EventQueue -> GraphicsMonad a ()
        sendEvent' (Just events) = liftIO $ writeChan events event
        sendEvent' Nothing = pure ()

goingToCloseWindow :: GraphicsMonad a ()
goingToCloseWindow = do
  old <- getInternalState
  let new = old { shouldCloseWindow = True }
  setInternalState new

endAnimation :: GraphicsMonad a ()
endAnimation = sendEvent Input.Close >> goingToCloseWindow

logStr :: String -> GraphicsMonad a ()
logStr = liftIO . hPutStr stderr
logLine :: String -> GraphicsMonad a ()
logLine = logStr . (++ "\n")
logWarn :: String -> GraphicsMonad a ()
logWarn = logLine . ("Acetone: [Warning]: " ++)