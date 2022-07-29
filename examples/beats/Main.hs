module Main where

import Acetone
import Acetone.Input
import Acetone.Shapes
import qualified Acetone.Backend.GLFW as GLFW

-- * -- Game Settings -- * --
-- Rate of recording and playback.
fps :: FPS
fps = 60
-- Max instantaneous BPM points remembered.
historySize :: Int
historySize = 16
-- Graphical BPM range.
bpmRange :: (Float, Float)
bpmRange = (20, 140)
-- Total number of ticks on y-axis.
ticks :: Int
ticks = 120
majorTicks :: Int
majorTicks = 20  -- Every 20 ticks get a text label

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
    openWindow "Beats" (1280, 720) (0, 0)
    GLFW.clearColor background
    -- NOTE: This seems like a common enough pattern.
    -- Maybe we should make a setup function which just takes
    -- the `updateState`, `eventHandler` and `drawScene` functions,
    -- and package that all into a function which gets taken by animationFrame.
    -- (Maybe we should still let drawScene be called manually, last, so we
    -- can allow controlling arguments which it takes, as long as it just gives a Picture)
    animationFrame fps (\ε -> pollEvents >>= handleEvents (updateState ε) >>= (\s -> setState s >> pure s) >>= drawScene)
    where updateState :: Elapsed -> Game State
          updateState ε = (\(State time beats) -> State (time + ε) beats) <$> getState
          handleEvents :: Game State -> [Event] -> Game State
          handleEvents = foldl $ \state event -> state >>= flip eventHandler event

eventHandler :: State -> Event -> Game State
eventHandler state@(State elapsed beats) = handle where
  -- Updates beat record when a new beat is played.
  recordBeat :: Beats -> Beats
  recordBeat beat
    = beat{ beatCount = succ $ beatCount beat
          , instantBPMs = take historySize $ instantBPM : instantBPMs beat
          , oldBPM = newBPM beat
          , newBPM = bpm
          , timeOfLastBeat = timeOfBeat beat
          , timeOfBeat = elapsed
          }
    where bpm = fromIntegral (fps * beatCount beat) / timeOfBeat beat
          instantBPM = fromIntegral fps / (timeOfBeat beat - timeOfLastBeat beat)
  handle :: Event -> Game State
  -- Record a beat.
  handle (Input Pressed (Mouse LeftClick) _) = pure $ State elapsed (recordBeat beats)
  handle (Input Pressed (Key KeySpace) _)    = pure $ State elapsed (recordBeat beats)
  -- Quit.
  handle (Input Released (Key (KeyChar 'Q')) _) = endAnimation >> pure state
  handle (Input Released (Key     KeyEscape) _) = endAnimation >> pure state
  handle Close = endAnimation >> pure state
  -- Reset the history.
  handle (Input Released (Key (KeyChar 'r')) _) = pure def
  handle _ = pure state

drawScene :: State -> Game Picture
drawScene (State elapsed beats) = do
  centre <- (,) <$> ofWidth 0.5 <*> ofHeight 0.5
  pure $ mconcat
    [ drawBeatCircle centre (elapsed - timeOfBeat beats) (1 / newBPM beats)
    ]

-- Take time between time since hit and now (growing) and the mean beat duration (1/BPM).
-- Produces a circle shrinking in size since time beat.
drawBeatCircle :: Point -> Elapsed -> Elapsed -> Picture
drawBeatCircle p sinceBeat beatDuration
  = transparent plum ((1 - clamp sinceBeat 0 1) / 4 + 0.2)
    `fill` circle p (0.2 + 0.7 * beatCurve beatDuration sinceBeat)

-- Sharply rising curve grows circle once beat was hit, slowly drops off.
beatCurve :: Double -> Double -> Double
beatCurve beatDuration x
  | x <= 0 = 0
  | x <= b / (2*r) = 2*r/b * x
  | x <= b = 1 / (1 - 1 / (2*r)) * (1 - x / b)
  | otherwise = 0
  where b = 30 * beatDuration - 0.1 -- b is duration of animation
        r = 2                       -- r is how sharply the circle radius rises.
