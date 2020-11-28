module Main where

import Shapes
import Render (render, defaultDomain)

red = staticColor $ color 1 0 0 0.5

test = [
    --drawing square red (rotate (pi/4) <+> (shear $ vec2 0 0))
    --drawing circle red (translate $ point 0.5 0.5)
    --drawing square red (scale $ vec2 0.5 0.5)
    --drawing square red (shear $ vec2 (pi/4) 0)
    --drawing square red ((translate $ point 0.5 0.5) <+> (scale $ vec2 0.5 0.5))
    drawing circle (staticColor $ color 1 0 0 0.5) (translate (point 0.5 0.5) <+> scale (point 0.5 0.4)),
    drawing circle (staticColor $ color 0 0 1 0.5) (translate (point 0.3 0.1) <+> scale (point 0.5 0.4)),
    drawing circle (staticColor $ color 0 1 0 0.5) (translate (point 0.1 0.2) <+> scale (point 0.4 0.5))
  ]

main = render "output.png" (defaultDomain (256, 256)) test
