{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Except
import Data.ByteString.UTF8
import Data.ByteString.Base64.URL
import Codec.Picture
import Graphics.Text.TrueType (loadFontFile)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty

import Render
import Templates

examples = [
  -- Just a red circle
  "[drawing circle (staticColor $ color 1 0 0 1) identity]",

  -- Overlapping circles with alpha blending
  "[drawing circle (staticColor $ color 1 0 0 0.5) (translate (point 0.5 0.5) <+> scale (point 0.5 0.4)),\n\
   \drawing circle (staticColor $ color 0 0 1 0.5) (translate (point 0.3 0.1) <+> scale (point 0.5 0.4)),\n\
   \drawing circle (staticColor $ color 0 1 0 0.5) (translate (point 0.1 0.2) <+> scale (point 0.4 0.5))]",

  -- Wacky mandelbrot
  "[drawing (mandelbrot 15) (gradientX (color 1 0 0 0) (color 0 1 0 0)) (translate (point 0.5 0) <+> scale (point 0.5 0.5) <+> shear (vec2 (pi/4) 0))]",

  "[drawing (polygon [point (-0.7) (-0.6), point 0.5 (-0.8), point 0.7 0.9, point (-0.9) 0.6, point (-0.3) (-0.1)]) (staticColor $ color 0 0 1 1) identity]"
  ]

errorFont = ExceptT $ loadFontFile "error.ttf"

main = runExceptT $ do
  ef <- errorFont

  lift $ scotty 3000 $ do
    get "/render/:expr/png" $ do
      exprEnc <- param "expr"
      img <- lift $ renderImageExpr (defaultDomain (512, 512)) ef 10 (toString $ decodeBase64Lenient exprEnc)

      setHeader "Content-Type" "image/png"
      raw $ encodePng $ img

    get "/" $
      html $ renderHtml $ home examples ""

    post "/" $ do
      expr <- param "expression"
      html $ renderHtml $ home examples expr
