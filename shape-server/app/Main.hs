module Main where

import Control.Monad.Except
import Codec.Picture
import Graphics.Text.TrueType (loadFontFile)

import Shapes
import Render

red = staticColor $ color 1 0 0 0.5
blue = staticColor $ color 0 0 1 0.7

cool = gradientX (color 1 0 0 0) (color 0 1 0 0)

test = [
    --drawing square red (rotate (pi/4) <+> (shear $ vec2 0 0))
    --drawing circle red (translate $ point 0.5 0.5)
    --drawing square red (scale $ vec2 0.5 0.5)
    --drawing square red (shear $ vec2 (pi/4) 0)
    --drawing square red ((translate $ point 0.5 0.5) <+> (scale $ vec2 0.5 0.5))
    drawing square cool ((translate $ point 0.3 0.3) <+> (scale $ point 0.5 0.5))
    --drawing (mandelbrot 20) (mandelbrotColor $ simpleMPalette 20 0.7) identity,
    --drawing (polygon [point (-0.7) (-0.6), point 0.5 (-0.8), point 0.7 0.9, point (-0.9) 0.6, point (-0.3) (-0.1)]) blue identity,
    --drawing circle (staticColor $ color 1 0 0 0.5) (translate (point 0.5 0.5) <+> scale (point 0.5 0.4)),
    --drawing circle (staticColor $ color 0 0 1 0.5) (translate (point 0.3 0.1) <+> scale (point 0.5 0.4)),
    --drawing circle (staticColor $ color 0 1 0 0.5) (translate (point 0.1 0.2) <+> scale (point 0.4 0.5))
  ]
--main = renderPngFile "output.png" (defaultDomain (256, 256)) test

testExpr = "let cool = gradientX (color 1 0 0 0) (color 0 0 1 0) in\n\
\  drawing square cool ((translate $ point 0.3 0.3) <+> (scale $ point 0.5 0.5))"

errorFont = ExceptT $ loadFontFile "error.ttf"

main = runExceptT $ do
  f <- errorFont
  img <- renderImageExpr (defaultDomain (1280, 720)) f 15 testExpr
  lift $ writePng "output.png" img
