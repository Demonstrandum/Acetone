module Acetone.Frontend where

-- | Front-end actions that indicate generally what actions
-- | the back-end should perform.
-- | These actions are general enough for a diverse set of
-- | back-ends with different use cases.
-- | These values are sent directly to the backend-callback
-- | along with the front-end internal state record.
data RendererAction
    = ClearBuffer  -- ^ Clear contents, called before a redraw.
    | ShowBuffer   -- ^ Swap buffers, called after a redraw.
    | DrawPicture  -- ^ Performs work needed to actually draw a `Picture`.
    | OpenSurface  -- ^ Opens a window, file or similar, specific to the backend.
    | Terminate    -- ^ Stop and cleanup the whole backend, not to be used again.
    | Init         -- ^ Called before the backend becomes useable, may be a noop.
    | DoNothing  -- &c.
    deriving Show