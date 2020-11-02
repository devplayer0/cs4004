module Main where

import Shapes
import Render (render,defaultWindow)

exampleDrawing =  [
    (scale (point 0.5 0.25) <+> translate (point 1.2 0.4),  circle),
    (scale (point 0.75 0.75) <+> translate (point (-0.5) (-0.75)), mandelbrot 50)
  ]

main = render "output.png" defaultWindow exampleDrawing
