{-|
    Module      : Acetone
    Description : Simple 2D graphics library with support for multiple backends.
    Licence     : GPL-3
    Maintainer  : Samuel <samuel@knutsen.co>
    Portability : Linux (X11/Wayland), macOS (Quartz), Windows (DWM)
|-}

module Acetone ( module Acetone
               , module Data.Default
               , liftIO
               ) where
-- Export all contents, and re-export some imports.

import Data.Default
import System.IO (hPutStr, stderr)
import Control.Arrow ((***))
import Control.Monad.State

import qualified Acetone.Input as Input
import qualified Acetone.Frontend as Frontend
import GHC.Clock (getMonotonicTime)
import Control.Concurrent (threadDelay, getChanContents, writeChan, readChan)
import qualified Data.Functor

-- | Used to represent pixel sizes internally.
type Pixels = Int
-- | Used for normalised distance.
type Distance = Double
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
    , closeWindow :: Bool
    , rendererAction :: Frontend.RendererAction
    , backendUpdate :: InternalState -> IO InternalState  -- ^ Only call with an intention (renderer action).
    , beforeRedraw :: IO ()
    ,  afterRedraw :: IO ()
    , getMousePosition :: IO (Double, Double)
    , getWindowPosition :: IO (Pixels, Pixels)
    , getWindowSize :: IO (Pixels, Pixels)
    , eventQueue :: Maybe Input.EventQueue  -- ^ Channel of events.
    }

instance Default InternalState where
    def = InternalState (pure ()) "" (0, 0) (0, 0) False Frontend.DoNothing
                        (\i -> hPutStr stderr "error: no backend installed!\n" >> pure i)
                        (hPutStr stderr "error: no buffer/window exists yet\n")
                        (hPutStr stderr "error: no buffer/window exists yet\n")
                        (pure (0, 0)) (pure (0, 0)) (pure (0, 0))
                        Nothing

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

normalizePixels :: Real n => (n, n) -> GraphicsMonad a (Distance, Distance)
normalizePixels (pixelsLeft, pixelsUp) = do
    internal <- getInternalState
    winSize <- liftIO $ getWindowSize internal
    let (w, h) = join (***) fromIntegral winSize
    pure (realToFrac pixelsLeft / w, realToFrac pixelsUp / h)

-- | Gets the mouse position in normalised units.
mousePosition :: GraphicsMonad a (Distance, Distance)
mousePosition = (getInternalState >>= (liftIO . getMousePosition)) >>= normalizePixels

--getState :: Member (State (InternalState, state)) a => Eff a (InternalState, state)
--getState = send getIt
--    where getIt = get :: State (InternalState, state) (InternalState, state)

--putState :: Member (State (InternalState, state)) a => (InternalState, state) -> Eff a ()
--putState = send . putIt
--    where putIt = put :: (InternalState, state) -> State (InternalState, state) ()

-- TODO: add non `Default` variants to functions?
--       so users don't have to create an instance for Default
--       and can pass the defaults directly in as appropriate.

-- This variant of openWindow requires state have a Default instance.
openWindow :: Default state => String -> (Int, Int) -> (Int, Int) -> GraphicsMonad state ()
openWindow title size pos = do
  i <- getInternalState
  let internal = i {
        windowTitle    = title
      , initialWindowSize     = size
      , initialWindowPosition = pos
      , rendererAction = Frontend.OpenWindow
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

monotonicMillis :: GraphicsMonad a Duration
monotonicMillis = (1000.0 *) <$> liftIO getMonotonicTime

animationFrame :: FPS -> (Elapsed -> GraphicsMonad a ()) -> GraphicsMonad a ()
animationFrame 0 _ = pure ()
animationFrame fps callback = monotonicMillis >>= animationFrame'
  where frameTime :: Elapsed
        frameTime = 1000.0 / fromIntegral fps
        animationFrame' yesterTime = do
          now <- monotonicMillis
          let elapsed = now - yesterTime

          pulseAction Frontend.ClearBuffer
          sendEvent Input.RenderedFrame  -- (!) must indicate end of last frame rendering.
          callback elapsed
          pulseAction Frontend.ShowBuffer

          let sleepyTime = frameTime - elapsed
          when (sleepyTime > 0) $
            liftIO $ threadDelay (round $ sleepyTime * 1000.0)

          stop <- closeWindow <$> getInternalState
          if stop then pure ()
                  else animationFrame' now

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

goingToCloseWindow = do
  old <- getInternalState
  let new = old { closeWindow = True }
  setInternalState new

endAnimation :: GraphicsMonad a ()
endAnimation = sendEvent Input.Close >> goingToCloseWindow

logStr :: String -> GraphicsMonad a ()
logStr = liftIO . hPutStr stderr
logLine :: String -> GraphicsMonad a ()
logLine = logStr . (++ "\n")
logWarn :: String -> GraphicsMonad a ()
logWarn = logLine . ("Acetone: [Warning]: " ++)