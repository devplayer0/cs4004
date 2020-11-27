module Render(Window, defaultDomain, sample, render) where
import Codec.Picture
import Shapes


--  A window specifies what part of the world to render and at which
--  resolution.
--  Values are top left & bottom right corner to be rendered,
--             and the size of the output device to render into
data Window = Window Point Point (Int, Int)

defaultDomain :: (Int, Int) -> Window
defaultDomain = Window (point (-1) (-1)) (point 1 1)

-- Generate an evenly spaced sample between two values (at the given position)
-- e.g. sample -1.5 +1.5 25 2 = -1.25
interpolate :: Double -> Double -> Int -> Int -> Double
interpolate c0 c1 n i = c0 + ((c1-c0) / (fromIntegral $ n-1) * fromIntegral i)

sample :: Int -> Int -> Double -> Int
sample a b d = (floor $ d * fromIntegral (b - a)) + a

-- Generate a point corresponding to the pixel of a window.
pixel :: Window -> (Int, Int) -> Point
pixel (Window tl br (w, h)) (i, j) =
  point x y where
    x = interpolate (getX tl) (getX br) w i
    y = interpolate (getY tl) (getY br) h j

rgba8 :: Color -> PixelRGBA8
rgba8 (Color r g b a) = PixelRGBA8 (s8b r) (s8b g) (s8b b) (s8b a)
  where s8b v = fromIntegral $ sample 0 255 v

-- render a drawing into an image, then save into a file
render :: String -> Window -> [Drawing] -> IO ()
render path win ds = writePng path $ generateImage pixRenderer w h
    where
      Window _ _ (w, h) = win

      pixRenderer x y =
        rgba8 $ (p `onlyInside` ds) `blendedColorAt` p
        where
          p = pixel win (x, y)
