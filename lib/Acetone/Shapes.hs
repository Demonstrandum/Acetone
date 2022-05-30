module Acetone.Shapes where

tau :: Double
tau = 2 * pi

halfPi :: Double
halfPi = pi / 2

-- | Used for normalised distance.
type Distance = Double
type Radius = Distance
type Point = Vertex
type Center = Point
type Radians = Distance

-- | A Cartesian coördinate pair of two doubles in [0; 1]./
type Vertex = (Distance, Distance)
-- | An RGBA-stored colour.
data Color = Color Double Double Double Double
-- | This is not supported yet.
type PixelBuffer = [[Color]]
-- | Linear, radial, conic, angular.
-- | Linear gradients must be provided with a direction (counter-clockwise from the x-axis)
-- | Radial, conic and angular gradients must provide a center to emanate from.
data GradientType = Linear Radians | Radial Center
                  | Conic Center   | Angular Center
data FillType = TrueSize | Tiled | FillStretched | FillWidth | FillHeight | Fill
-- | Colours, gradients and images.
data Texture = Solid Color
             -- | Array of colours in gradient, array of offsets (0..1) 
             -- | (which will be moduloed/wrapped) for each colour, and the gradient style. 
             | Gradient [Color] [Double] GradientType
             -- | Pixel buffer, how to fill the surface, and where to center it ((0,0) is center).
             | Buffer PixelBuffer FillType Point
-- | A single shape is a set of connected vertices (in order) with a fill and stroke colour.
-- | Shape should not need to be constructed directly.  They are a fragment of a picture.
-- | Shapes cannot be displayed on the scree, they need to be part of a `Picture` first.
-- | Convert a single shape to a picture by setting the fill or stroke colour with `fill`
-- | or `stroke`, or by just using `toPicture`.  Then the picture may be composed together
-- | with other pictures to form a more elaborate and complete scene.
data Shape = Shape [Vertex] Texture Texture
-- | A picture is the composition of multiple shapes.
-- | It derives semigroup and monoid. `mempty` is the blank picture,
-- | and `mappend` (`<>`) is picture concatenation/superimposition operator.
newtype Picture = Picture [Shape]
  deriving (Semigroup, Monoid)

-- | May be appropriately converted to a picture.
class ToPicture p where
  toPicture :: p -> Picture

instance ToPicture Picture where
  toPicture = id

instance ToPicture Shape where
  toPicture shape = Picture [shape]

instance ToPicture [Shape] where
  toPicture = Picture

-- | May be appropriately converted to a texture.
class ToTexture t where
  toTexture :: t -> Texture

instance ToTexture Texture where
  toTexture = id

instance ToTexture Color where
  toTexture = Solid

-- | Perform operation on each shape in a picture.
mapPicture :: (Shape -> Shape) -> Picture -> Picture
mapPicture _ empty@(Picture []) = empty
mapPicture f (Picture (shape:shapes)) = Picture [f shape] <> mapPicture f (Picture shapes)

unPicture :: Picture -> [Shape]
unPicture (Picture shapes) = shapes

-- | Set the fill colour for an entire a shape or picture.
fill :: ToTexture t => ToPicture p => t -> p -> Picture
fill texture = mapPicture (fillShape texture) . toPicture

-- | Set the fill colour for an entire a shape or picture.
stroke :: ToPicture p => Color -> p -> Picture
stroke color = mapPicture (strokeShape color) . toPicture

-- | Set fill colour of a shape.
fillShape :: ToTexture t => t -> Shape -> Shape
fillShape filling (Shape verts _ s) = Shape verts (toTexture filling) s

-- | Set stroke colour of a shape.
strokeShape :: ToTexture t => t -> Shape -> Shape
strokeShape filling (Shape verts f _) = Shape verts f (toTexture filling)

-- | Use `polygon` with `fill` and `stroke` functions instead.
newShape :: ToTexture t => [Vertex] -> t -> t -> Shape
newShape vertices filling stroking = Shape vertices (toTexture filling) (toTexture stroking)

-- | Shape with given vertices.
polygon :: [Vertex] -> Shape
polygon vertices = newShape vertices void black

-- | Line segment joining two points.
line :: Vertex -> Vertex -> Shape
line p0 p1 = polygon [p0, p1]

-- | Close a shape by duplicating and prepending the last vertex.
close :: Shape -> Shape
close (Shape vs f s) = Shape (last vs : vs) f s

-- | Ellipse given the major and minor axes, and a centre.
ellipse :: Distance -> Distance -> Point -> Shape
ellipse major minor (x, y) = close $ polygon vertices
  where vertices = [ (x + major * cos t, y + minor * sin t) | t <- [0, 0.05 .. tau] ]

-- | Circle of radius r with given centre.
circle :: Distance -> Point -> Shape
circle r = ellipse r r

pointwise :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
pointwise f g (x, y) = (f x, g y)

-- | Translate shape such that an origin point becomes the provided point.
translateShape :: Point -> Shape -> Shape
translateShape (dx, dy) (Shape v f s) = Shape (map (pointwise (+ dx) (+ dy)) v) f s

-- | Rotate a whole picture together around a common centre.
rotate :: ToPicture p => Radians -> Point -> p -> Picture
rotate angle centre = mapPicture (rotateShape angle centre) . toPicture

-- | Rotate a shape about a centre.
rotateShape :: Radians -> Point -> Shape -> Shape
rotateShape angle (x, y) = translateShape (x, y) . rotateShapeOrigin angle . translateShape (-x, -y)

-- | Rotate a shape, always about the origin.
rotateShapeOrigin :: Radians -> Shape -> Shape
rotateShapeOrigin t (Shape vs f s) = Shape vertices f s
  where vertices = [(x * cos t - y * sin t, x * sin t + y * cos t) | (x, y) <- vs]

white :: Color
white = Color 1 1 1 1
black :: Color
black = Color 0 0 0 1
void :: Color
void = Color 0 0 0 0
orange :: Color
orange = Color 1 0.5 0 1
plum :: Color
plum = Color 0.67 0.194 0.255 1

-- | Make a colour opaque, i.e. removes transparency.
solid :: Color -> Color
solid (Color r g b _) = Color r g b 1
opaque :: Color -> Color  -- ^ Alias to `solid`.
opaque = solid

-- | Give transparency to colour.
-- | If colour is already transparent, the alpha channels multiply.
transparent :: Color -> Double -> Color
transparent (Color r g b a) alpha = Color r g b $ a * alpha