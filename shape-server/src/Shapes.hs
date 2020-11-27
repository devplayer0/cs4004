module Shapes(
  Shape, Point, Vector, Transform,
  ColorMap, Color(Color), Coloring, Drawing,

  point, getX, getY,
  empty, circle, square, mandelbrot, polygon,
  mNext, mSeries, fairlyClose, inMandelbrotSet, approxInMandelbrot,
  identity, translate, rotate, scale, (<+>), transform,
  distance, maxnorm, intersects,
  color, blend, staticColor, mappedColor, blendedColor,
  drawing, inside, onlyInside, colorAt, blendedColorAt,
  ) where


-- Utilities

data Vector = Vector Double Double
              deriving Show
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x _) = x
getY (Vector _ y) = y

-- Shapes

type Point = Vector

point :: Double -> Double -> Point
point = vector


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
mNext (Vector u v) (Vector x y) = Vector (x * x - y * y + u) (2 * x * y + v)

mSeries :: Point -> [Point]
mSeries p = iterate (mNext p) (Vector 0 0)

fairlyClose :: Point -> Bool
fairlyClose (Vector u v) = (u*u + v*v) < 100

inMandelbrotSet :: Point -> Bool
inMandelbrotSet p = all fairlyClose (mSeries p)

approxInMandelbrot :: Int -> Point -> Bool
approxInMandelbrot n p = all fairlyClose (take n (mSeries p))

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Matrix
             deriving Show

identity = Identity
translate = Translate
scale = Scale
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = id x
transform (Translate (Vector tx ty)) (Vector px py) = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py) = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = (invert m) `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

distance :: Point -> Double
distance (Vector x y) = sqrt (x**2 + y**2)

maxnorm :: Point -> Double
maxnorm (Vector x y) = max (abs x) (abs y)

intersects :: Point -> Shape -> Bool
_ `intersects` Empty        = False
p `intersects` Circle       = distance p <= 1
p `intersects` Square       = maxnorm  p <= 1
p `intersects` Mandelbrot n = approxInMandelbrot n p

type Alpha = Double
data Color = Color Double Double Double Alpha
             deriving Show
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

data Drawing = Drawing Shape Coloring Transform
drawing = Drawing

inside :: Point -> Drawing -> Bool
inside p (Drawing s _ t) = intersects (transform t p) s

onlyInside :: Point -> [Drawing] -> [Drawing]
onlyInside p d = filter (inside p) d

colorAt :: Drawing -> Point -> Color
colorAt (Drawing _ (Static c) _) _ = c
colorAt (Drawing s (Custom mapper) t) p = mapper s $ transform t p
colorAt d p = blend (colorAt da p) (colorAt db p)
    where Drawing s (Blended ca cb) t = d
          da = Drawing s ca t
          db = Drawing s cb t

blendedColorAt :: [Drawing] -> Point -> Color
blendedColorAt [] _ = color 0 0 0 0
blendedColorAt (d:ds) p = blend (d `colorAt` p) (ds `blendedColorAt` p)
