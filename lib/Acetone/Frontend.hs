module Acetone.Frontend where

-- | Front-end actions that indicate generally what actions
-- | the back-end should perform.
-- | These actions are general enough for a diverse set of
-- | back-ends with different use cases.
-- | These values are sent directly to the backend-callback
-- | along with the front-end internal state record.
data RendererAction
    = ClearBuffer  -- ^ clear contents, called before a redraw
    | ShowBuffer   -- ^ swap buffers, called after a redraw
    | DrawPicture
    | OpenWindow
    | Terminate
    | Init
    | DoNothing  -- &c.
    deriving Show