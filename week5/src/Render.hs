module Render(Window,defaultWindow,sample,render) where
import Codec.Picture
import Shapes


--  A window specifies what part of the world to render and at which
--  resolution.
--  Values are top left & bottom right corner to be rendered,
--             and the size of the output device to render into
data Window = Window Point Point (Int, Int)

-- Default window renders a small region around the origin into
-- a 50x50 pixel image
defaultWindow :: Window
defaultWindow = Window (point (-1.5) (-1.5)) (point 1.5 1.5) (512, 512)

-- Generate an evenly spaced sample between two values (at the given position)
-- e.g. sample -1.5 +1.5 25 2 = -1.25
sample :: Double -> Double -> Int -> Int -> Double
sample c0 c1 n i = c0 + ((c1-c0) / (fromIntegral $ n-1) * fromIntegral i)

-- Generate a point corresponding to the pixel of a window.
pixel :: Window -> (Int, Int) -> Point
pixel (Window p0 p1 (w, h)) (i, j) =
  point x y where
    x = sample (getX p0) (getX p1) w i
    y = sample (getY p0) (getY p1) h j

-- render a drawing into an image, then save into a file
render :: String -> Window -> Drawing -> IO ()
render path win sh = writePng path $ generateImage pixRenderer w h
    where
      Window _ _ (w,h) = win

      pixRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) (blueForPoint $ pixel win (x, y))

      blueForPoint :: Point -> Pixel8
      blueForPoint p | p `inside` sh = 255
                     | otherwise     = 0
