module Main where

import Acetone
import Acetone.Input
import Acetone.Shapes
import Acetone.Backend.GLFW (backend)

-- Our game state with the position of circles.
data State = State [(Distance, Distance)] (Distance, Distance)

instance Default State where
    def = State [] (0, 0)

-- Drawing context monad with our above state.
type Context = GraphicsMonad State

main :: IO ()
main = setupGraphics initExample

initExample :: Context ()
initExample = installBackend backend
           >> openWindow "Example" (720, 480) (0, 0)
           >> animationFrame 60 (\elapsed -> pollEvents >>= mapM_ eventHandler >> drawScene elapsed)

eventHandler :: Event -> Context ()
eventHandler (Input Pressed (Mouse LeftClick) _) = circleWhereClick
eventHandler (Input Released (Key (KeyChar 'Q')) _) = endAnimation
eventHandler (MousePosition x y) = setHoverCircle x y
eventHandler Close = endAnimation
eventHandler _ = pure ()

drawScene :: Elapsed -> Context ()
drawScene elapsed = do
  (State circles (x, y)) <- getState
  let cursorDisk = fill (transparent orange 0.4) $ circle 0.035 (x, y)
  draw $ (foldr (<>) mempty $ map (fill white . circle 0.03) circles) <> cursorDisk

setHoverCircle :: Distance -> Distance -> Context ()
setHoverCircle x y = do
  (State xs _) <- getState
  setState (State xs (x, y))

circleWhereClick :: Context ()
circleWhereClick = do
    x <- mousePosition
    logLine $ "\n\nput circle at: " ++ show x
    (State xs cursor) <- getState
    setState (State (x:xs) cursor)
