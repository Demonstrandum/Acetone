module Main where

import Acetone
import Acetone.Input
import Acetone.Backend.GLFW (backend)

-- Position of circles.
data State = State [(Distance, Distance)]
instance Default State where
    def = State []

-- Drawing context monad with our above state.
type Context = GraphicsMonad State

main :: IO ()
main = setupGraphics initExample

initExample :: Context ()
initExample = installBackend backend
           >> openWindow "Example" (720, 480) (0, 0)
           >> animationFrame 60 (\elapsed -> pollEvents >>= mapM_ eventHandler >> drawScene elapsed)

eventHandler :: Event -> Context ()
eventHandler (Input Pressed (Mouse LeftClick) _) = circleUnderMouse
eventHandler (Input Released (Key (KeyChar 'Q')) _) = endAnimation
eventHandler (Resize w h) = logLine $ "Resized to:" ++ show (w, h)
eventHandler (MousePosition x y) = logStr (" mouse over: " ++ show (x, y))
--eventHandler Close = 
eventHandler _ = pure ()

drawScene :: Elapsed -> Context ()
drawScene elapsed = logStr $ "\r (.) drew (frame time: " ++ show elapsed ++ ")"
--    (State circles) <- getState
--    fold (>>) $ map drawCircle circles

--drawScene NoChange = pure NoChange

-- Get mousePositions out of Context state and add them to out state's array
circleUnderMouse :: Context ()
circleUnderMouse = do
    x <- mousePosition
    liftIO $ putStrLn ("mouse position:" ++ show x)
    (State xs) <- getState
    setState (State (x:xs))

--drawCircle :: (Distance, Distance) -> Context ()
--drawCircle (x, y) = solid white >> circle x y