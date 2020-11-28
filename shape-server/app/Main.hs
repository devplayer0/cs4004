module Main where

import Data.List

import Control.Monad.Catch
import qualified Language.Haskell.Interpreter as Hint

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
\  [drawing square cool ((translate $ point 0.3 0.3) <+> (scale $ point 0.5 0.5))]"

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

main = do
  r <- evalPicture testExpr
  case r of
    Left err -> putStrLn $ errorString err
    Right p ->
      renderPngFile "output.png" (defaultDomain (256, 256)) p
