module Render(
  Window,

  defaultDomain, pixel,
  rgba8, renderPixel, renderImage, renderPng, renderPngFile,
  drawLines, renderError, evalPicture, renderImageExpr,
  ) where

import Data.List
import Data.List.Split
import Control.Monad.Catch

import Codec.Picture
import qualified Language.Haskell.Interpreter as Hint
-- Just for text rendering!
import Graphics.Text.TrueType (Font, loadFontFile)
import qualified Graphics.Rasterific as Rasterific
import qualified Graphics.Rasterific.Texture as RasterificTexture

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

drawLines _ _ [] _ = return ()
drawLines font size (l:ls) (Rasterific.V2 x y) =
  let np = (Rasterific.V2 x (2 + (Rasterific.getPointSize size) + y)) in do
    Rasterific.printTextAt font size np l
    drawLines font size ls np

renderError :: Window -> Font -> Float -> String -> Image PixelRGBA8
renderError win font size msg =
  Rasterific.renderDrawing w h (PixelRGBA8 255 255 255 255)
    . Rasterific.withTexture (RasterificTexture.uniformTexture $ PixelRGBA8 255 0 0 255) $
        drawLines font (Rasterific.PointSize size) (splitOn "\n" msg) (Rasterific.V2 0 0)
  where Window _ _ (w, h) = win

renderPng win pic = encodePng $ renderImage win pic

renderPngFile path win pic = writePng path $ renderImage win pic

errorString :: Hint.InterpreterError -> String
errorString (Hint.WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (Hint.GhcError e) = e
errorString e = show e

evalPicture :: (Hint.MonadIO m, MonadMask m) => String -> m (Either Hint.InterpreterError Picture)
evalPicture s = Hint.runInterpreter $ do
  Hint.setImports ["Prelude", "Math", "Shapes"]
  Hint.interpret s (Hint.as :: Picture)

renderImageExpr :: (Hint.MonadIO m, MonadMask m) => Window -> Font -> Float -> String -> m (Image PixelRGBA8)
renderImageExpr win errFont errFontSize expr = do
  r <- evalPicture expr
  return $ case r of
    Left err -> renderError win errFont errFontSize (errorString err)
    Right p -> renderImage win p
