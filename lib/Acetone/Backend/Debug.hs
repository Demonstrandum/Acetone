-- | This is a nonsense 'debug' backend, that only prints to the console.
module Acetone.Backend.Debug where

import Data.Default
import System.IO (hPutStrLn, stderr)

import qualified Acetone as Ac
import Acetone.Frontend

action :: String -> IO ()
action = hPutStrLn stderr

-- backend function is called each time there is a change of state
-- that the backend (renderer) need to be aware of / act on.
backend :: Ac.InternalState -> IO Ac.InternalState
backend i = do
    case Ac.rendererAction i of
        Init -> action $ "-| initialising backend"
        OpenWindow ->
            action $ "-| opened window [" ++ Ac.windowTitle i ++ "] " ++ (show . Ac.windowSize) i
        DoNothing -> pure ()
        ClearBuffer -> action $ "-| cleared drawing surface"
        ShowBuffer  -> action $ "-| displaying drawing surface"
    pure i
    -- hook into local backend function to perform the given state
