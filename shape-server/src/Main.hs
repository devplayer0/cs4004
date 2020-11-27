module Main where

import Shapes
import Render (render, defaultDomain)

test = [
    drawing circle (staticColor $ color 1 0 0 0.5) (translate (point 0.5 0.5) <+> scale (point 0.5 0.4)),
    drawing circle (staticColor $ color 0 0 1 0.5) (translate (point 0.3 0.1) <+> scale (point 0.5 0.4)),
    drawing circle (staticColor $ color 0 1 0 0.5) (translate (point 0.1 0.2) <+> scale (point 0.4 0.5))
  ]

main = render "output.png" (defaultDomain (512, 512)) test
