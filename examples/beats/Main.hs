module Main where

import Acetone
import Acetone.Input
import Acetone.Shapes
import qualified Acetone.Backend.GLFW as GLFW

import Text.Printf
import System.IO (hFlush, stdout)

-- * -- Game Settings -- * --
-- Rate of recording and playback.
fps :: FPS
fps = 120
-- Max instantaneous BPM points remembered.
historySize :: Int
historySize = 16
-- Graphical BPM range.
bpmRange :: (Double, Double)
bpmRange = (20, 140)
-- Total number of ticks on y-axis.
ticks :: Int
ticks = 120
majorTicks :: Int
majorTicks = 20  -- Every 20 ticks get a text label
-- Screen border padding
paddingX :: Distance
paddingX = 0.01
paddingY :: Distance
paddingY = 0.03

-- Our program state with the frame time and BPM information.
data State = State Elapsed Beats deriving Show
-- Stats on beats.
data Beats
  = Beats { beatCount :: Int
          , instantBPMs :: [Double]
          , oldBPM :: Double
          , newBPM :: Double
          , timeOfLastBeat :: Elapsed
          , timeOfBeat :: Elapsed
          }
    deriving Show

instance Default State where
    def = State 0 def
instance Default Beats where
  def = Beats { beatCount = 0
              , instantBPMs = replicate historySize 60
              , oldBPM = 60
              , newBPM = 60
              , timeOfLastBeat = 0
              , timeOfBeat = 0
              }

-- Drawing context monad with our above state.
type Game = GraphicsMonad State

primary :: Color
primary = Color 0.67 0.194 0.255 1
background :: Color
background = Color 0.175 0.0212 0.0212 1

main :: IO ()
main = setupGraphics initGame where
  initGame = do
    installBackend GLFW.backend
    openSurface "Beats" (1280, 720) (0, 0)
    GLFW.clearColor background  --  TODO: provide a agnostic way of doing this, dynamically too.
    -- NOTE: This seems like a common enough pattern.
    -- Maybe we should make a setup function which just takes
    -- the `updateState`, `eventHandler` and `drawScene` functions,
    -- and package that all into a function which gets taken by animationFrame.
    -- (Maybe we should still let drawScene be called manually, last, so we
    -- can allow controlling arguments which it takes, as long as it just gives a Picture)
    logLine "Running Beats!\n"
    simulationLoop fps (\old ε -> debug ε >> pollEvents >>= handleEvents (updateState old ε) >>= drawScene)
    where updateState :: State -> Elapsed -> State
          updateState (State time beats) ε = State (time + ε) beats
          handleEvents :: State -> [Event] -> Game State
          handleEvents = foldl (\state event -> state >>= flip eventHandler event) . pure
          debug eps = liftIO $ printf "\r:: ift: %0.2f; fps: %0.0f" eps (1000/eps) >> hFlush stdout

eventHandler :: State -> Event -> Game State
eventHandler state@(State elapsed beats) = handle where
  -- Updates beat record when a new beat is played.
  recordBeat :: Beats -> Beats
  recordBeat beat
    = beat{ beatCount = succ $ beatCount beat
          , instantBPMs = take historySize $ instantBPM : instantBPMs beat
          , oldBPM = newBPM beat
          , newBPM = averageBPM
          , timeOfLastBeat = timeOfBeat beat
          , timeOfBeat = elapsed
          }
    where averageBPM = 60 * 1000 * fromIntegral (beatCount beat) / timeOfBeat beat
          instantBPM = 60 * 1000 / (timeOfBeat beat - timeOfLastBeat beat)
  handle :: Event -> Game State
  -- Record a beat.
  handle (Input Pressed (Mouse LeftClick) _) = pure $ State elapsed (recordBeat beats)
  handle (Input Pressed (Key KeySpace) _)    = pure $ State elapsed (recordBeat beats)
  -- Quit.
  handle (Input Released (Key [char| q |]) _) = endAnimation >> pure state
  handle (Input Released (Key KeyEscape) _) = endAnimation >> pure state
  handle Close = endAnimation >> pure state
  -- Reset the history.
  handle (Input Released (Key [char| R |]) _) = pure def
  handle _ = pure state

drawScene :: State -> Game (State, Picture)
drawScene state@(State elapsed beats) = do
  wide <- ofWidth 1
  tall <- ofHeight 1
  centre <- (,) <$> ofWidth 0.5 <*> ofHeight 0.5
  let scene = drawBeatCircle centre (elapsed - timeOfBeat beats) (1000 / newBPM beats)
           <> drawHistory wide tall (instantBPMs beats)
           <> drawBpmLine wide tall (elapsed - timeOfBeat beats) (oldBPM beats) (newBPM beats)
           <> drawAxisTicks wide tall
  pure (state, scene)

-- Take time between time since hit and now (growing) and the mean beat duration (1/BPM).
-- Produces a circle shrinking in size since time beat.
drawBeatCircle :: Point -> Elapsed -> Elapsed -> Picture
drawBeatCircle p sinceBeat beatDuration
  = transparent (0.8 - clamp 0 0.7 (sinceBeat / 300)) plum
    `fill` circle p (0.2 + 0.7 * beatCurve beatDuration sinceBeat)

drawBpmLine :: Distance -> Double -> Double -> Double -> Double -> Picture
drawBpmLine w h sinceBeat oldBpm bpm = fill (transparent 0.5 white) $ line (0, ybpm) (w, ybpm)
  where lerpSpeed = 0.005  -- distance units per ms
        lerpBpm = oldBpm + (bpm - oldBpm) * clamp 0 1 (sinceBeat * lerpSpeed)
        ybpm = uncurry align bpmRange h 0 lerpBpm

drawAxisTicks :: Distance -> Distance -> Picture
drawAxisTicks w h = mconcat [ drawAxisTick tick (w, h) | tick <- [0..ticks] ]

drawAxisTick :: Int -> (Distance, Distance) -> Picture
drawAxisTick tick (_w, h)
  -- major tick
  | tick `mod` majorTicks == 0
    =  fill (transparent 0.9 white) $ line (paddingX, y) (0.03 + paddingX, y)
    -- <> text (-1 / 2, 0.1) 0.3 (show label)
  -- minor tick
  | otherwise = fill (transparent 0.8 white) $ line (paddingX, y) (0.01 + paddingX, y)
  where y = align 0 (fromIntegral ticks) (h - paddingY) (0 + paddingY) (fromIntegral tick)
        -- label = round $ uncurry (align 0 (fromIntegral ticks)) bpmRange (fromIntegral tick)

drawHistory :: Distance -> Distance -> [Double] -> Picture
drawHistory w h history = mconcat [ drawBpmPoint n bpm | (n, bpm) <- zip [0..] history ]
  where drawBpmPoint :: Int -> Double -> Picture
        drawBpmPoint n bpm = fill (transparent 0.6 white) $ circle (x, y) r where
          r = 0.005
          y = uncurry align bpmRange (h - paddingY) paddingY bpm
          x = align 0 (fromIntegral historySize) 0.1 w (fromIntegral n)

-- Sharply rising curve grows circle once beat was hit, slowly drops off.
beatCurve :: Double -> Double -> Double
beatCurve beatDuration x
  | x <= 0 = 0
  | x <= b / (2*r) = 2*r/b * x
  | x <= b = 1 / (1 - 1 / (2*r)) * (1 - x / b)
  | otherwise = 0
  where b = 30 * beatDuration - 100  -- b is duration of animation
        r = 2                        -- r is how sharply the circle radius rises.
