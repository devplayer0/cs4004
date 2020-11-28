module Shapes(
  Point, Line, Transform(Affine), Shape,
  Alpha, Color(Color), ColorMap, Coloring,
  Drawing, Picture,

  line, point, identity, translate, scale, affine, rotate, shear, (<+>), applyAffine, reduceTransform, transform,

  empty, circle, square, mandelbrot, polygon,
  mNext, mSeries, fairlyClose, inMandelbrotSet, approxInMandelbrot,

  distance, maxnorm, onLine, intersects,

  color, blend, staticColor, mappedColor, blendedColor,
  gradientX,
  drawing, inside, onlyInside, colorAt, blendedColorAt,
  ) where

import Math

type Point = Vector

point :: Double -> Double -> Point
point = vec2

data Line = Line Point Point
line = Line

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Rotate Double
           | Shear Vector
           | Affine Matrix
           | Compose Transform Transform
             deriving Show

identity = Identity
translate = Translate
scale = Scale
rotate = Rotate
shear = Shear
affine = Affine
t0 <+> t1 = Compose t0 t1

applyAffine :: Matrix -> Point -> Point
t `applyAffine` (Vec2 x y) = let (Vec3 tx ty _) = (invert t) `mv` vec3 x y 1 in vec2 tx ty

reduceTransform :: Transform -> Transform
reduceTransform (Affine m)               = Affine m
reduceTransform Identity                 = Affine ident3
reduceTransform (Translate (Vec2 tx ty)) = Affine $ mat3 (vec3 1 0 0) (vec3 0 1 0) (vec3 tx ty 1)
reduceTransform (Scale (Vec2 sx sy))     = Affine $ mat2to3 $ mat2 (vec2 sx 0) (vec2 0 sy)
reduceTransform (Rotate a)               = Affine $ mat2to3 $ mat2 (vec2 (cos a) (-sin a)) (vec2 (sin a) (cos a))
reduceTransform (Shear (Vec2 ax ay))     = Affine $ mat2to3 $ mat2 (vec2 1 (tan ay)) (vec2 (tan ax) 1)
reduceTransform (Compose a b)            = Affine $ ma `mm` mb
                                           where (Affine ma) = (reduceTransform a)
                                                 (Affine mb) = (reduceTransform b)

transform :: Transform -> Point -> Point
--t `transform` _ | trace (show t) False = undefined
--t `transform` p | trace (show (m `applyAffine` p)) False = undefined
--  where (Affine m) = reduceTransform t
t `transform` p = m `applyAffine` p
  where (Affine m) = reduceTransform t

data Shape = Empty
           | Circle
           | Square
           | Mandelbrot Int
           | Polygon [Point]
             deriving Show

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square
mandelbrot n = Mandelbrot n
polygon ps = Polygon ps

mNext :: Point -> Point -> Point
mNext (Vec2 u v) (Vec2 x y) = point (x * x - y * y + u) (2 * x * y + v)

mSeries :: Point -> [Point]
mSeries p = iterate (mNext p) (point 0 0)

fairlyClose :: Point -> Bool
fairlyClose (Vec2 u v) = (u*u + v*v) < 100

inMandelbrotSet :: Point -> Bool
inMandelbrotSet p = all fairlyClose (mSeries p)

approxInMandelbrot :: Int -> Point -> Bool
approxInMandelbrot n p = all fairlyClose (take n (mSeries p))

chooseMColor :: [a] -> [Point] -> a
chooseMColor palette = (palette !!) . length . take n . takeWhile fairlyClose
  where n = length palette - 1

distance :: Point -> Double
distance (Vec2 x y) = sqrt (x**2 + y**2)
distance (Vec3 x y z) = sqrt (x**2 + y**2 + z**2)

maxnorm :: Point -> Double
maxnorm (Vec2 x y) = max (abs x) (abs y)

onLine :: Point -> Line -> Bool
(Vec2 px py) `onLine` (Line (Vec2 ax ay) (Vec2 bx by)) =
  px <= max ax bx &&
  px >= min ax bx &&
  py <= max ay by &&
  py >= min ay by

-- https://wrf.ecse.rpi.edu/Research/Short_Notes/pnpoly.html
insidePoly' :: Point -> Point -> [Point] -> Bool -> Bool
insidePoly' _ _ [] i = i
insidePoly' p (Vec2 jx jy) (nextJ:vs) inside = insidePoly' p nextJ vs nextInside
  where
    (Vec2 px py) = p
    (Vec2 ix iy) = nextJ

    nextInside =
      if (iy > py) /= (jy > py) && px < (jx - ix) * (py - iy) / (jy - iy) + ix
        then not inside
        else     inside

intersects :: Point -> Shape -> Bool
_ `intersects` Empty        = False
p `intersects` Circle       = distance p <= 1
p `intersects` Square       = maxnorm  p <= 1
p `intersects` Mandelbrot n = approxInMandelbrot n p
p `intersects` Polygon ps   = insidePoly' p (last ps) ps False

type Alpha = Double
data Color = Color {
    r :: Double
  , g :: Double
  , b :: Double
  , a :: Alpha
  }
color = Color

blend :: Color -> Color -> Color
blend (Color r0 g0 b0 a0) (Color r1 g1 b1 a1) =
  Color r01 g01 b01 a01
  where a01 = (1-a0)*a1+a0
        r01 = ((1-a0)*a1*r1+a0*r0)/a01
        g01 = ((1-a0)*a1*g1+a0*g0)/a01
        b01 = ((1-a0)*a1*b1+a0*b0)/a01

type ColorMap = Shape -> Point -> Color
data Coloring = Static Color
              | Custom ColorMap
              | Blended Coloring Coloring
staticColor = Static
mappedColor = Custom
blendedColor = Blended

gradientX a b = mappedColor $ \_ (Vec2 x _) -> let nx = mapDom (-1) 1 x in blend (a { a = nx }) (b { a = 1 - nx })

data Drawing = Drawing Shape Coloring Transform
drawing = Drawing

type Picture = [Drawing]

inside :: Point -> Drawing -> Bool
inside p (Drawing s _ t) = (t `transform` p) `intersects` s

onlyInside :: Point -> Picture -> Picture
onlyInside p d = filter (inside p) d

colorAt :: Drawing -> Point -> Color
colorAt (Drawing _ (Static c) _) _ = c
colorAt (Drawing s (Custom mapper) t) p = mapper s $ t `transform` p
colorAt d p = blend (colorAt da p) (colorAt db p)
    where Drawing s (Blended ca cb) t = d
          da = Drawing s ca t
          db = Drawing s cb t

blendedColorAt :: Picture -> Point -> Color
blendedColorAt [] _ = color 0 0 0 0
blendedColorAt (d:ds) p = blend (d `colorAt` p) (ds `blendedColorAt` p)
