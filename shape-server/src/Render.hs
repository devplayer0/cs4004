module Render(
  Window,

  defaultDomain, pixel,
  rgba8, renderPixel, renderImage, renderPng, renderPngFile
  ) where

import Codec.Picture

import Math
import Shapes

--  A window specifies what part of the world to render and at which
--  resolution.
--  Values are top left & bottom right corner to be rendered,
--             and the size of the output device to render into
data Window = Window Point Point (Int, Int)

defaultDomain :: (Int, Int) -> Window
defaultDomain = Window (point (-1) (-1)) (point 1 1)

-- Generate a point corresponding to the pixel of a window.
pixel :: Window -> (Int, Int) -> Point
pixel (Window tl br (w, h)) (i, j) =
  point x y where
    x = interpolate (getX tl) (getX br) w i
    y = interpolate (getY tl) (getY br) h j

rgba8 :: Color -> PixelRGBA8
rgba8 (Color r g b a) = PixelRGBA8 (s8b r) (s8b g) (s8b b) (s8b a)
  where s8b v = fromIntegral $ sample 0 255 v

renderPixel win pic x y =
        rgba8 $ (p `onlyInside` pic) `blendedColorAt` p
        where
          p = pixel win (x, y)

renderImage win pic = generateImage (renderPixel win pic) w h
  where Window _ _ (w, h) = win

renderPng win pic = encodePng $ renderImage win pic

renderPngFile path win pic = writePng path $ renderImage win pic
