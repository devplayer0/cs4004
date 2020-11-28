{-# LANGUAGE DataKinds #-}
module Shapes(
  Shape, Point, Vector, Matrix, Transform(Affine),
  ColorMap, Color(Color), Coloring, Drawing,

  vec2, vec3, getX, getY,
  mat2, mat3, ident2, ident3, mat2to3, transpose,
  dot, mf, mv, mm, det, adj, invert,

  point, identity, translate, scale, affine, rotate, shear, (<+>), applyAffine, reduceTransform, transform,
  empty, circle, square, mandelbrot, polygon,
  mNext, mSeries, fairlyClose, inMandelbrotSet, approxInMandelbrot,
  distance, maxnorm, intersects,
  color, blend, staticColor, mappedColor, blendedColor,
  drawing, inside, onlyInside, colorAt, blendedColorAt,
  ) where

import Debug.Trace

-- Column Vector type
data Vector = Vec2 Double Double
            | Vec3 Double Double Double
              deriving Show
vec2 = Vec2
vec3 = Vec3

getX (Vec2 x _) = x
getY (Vec2 _ y) = y

data Matrix = Mat2 Vector Vector
            | Mat3 Vector Vector Vector
              deriving Show

mat2 = Mat2
mat3 = Mat3

ident2 = Mat2 (vec2 1 0) (vec2 0 1)
ident3 = mat2to3 ident2
mat2to3 :: Matrix -> Matrix
mat2to3 (Mat2 (Vec2 a b) (Vec2 c d)) = mat3 (vec3 a b 0) (vec3 c d 0) (vec3 0 0 1)

transpose :: Matrix -> Matrix
transpose (Mat2 (Vec2 a b) (Vec2 c d)) = mat2 (vec2 a c) (vec2 b d)
transpose (Mat3 (Vec3 a b c) (Vec3 d e f) (Vec3 g h i)) = mat3 (vec3 a d g) (vec3 b e h) (vec3 c f i)

dot :: Vector -> Double -> Vector
(Vec2 a b)   `dot` f = vec2 (a*f) (b*f)
(Vec3 a b c) `dot` f = vec3 (a*f) (b*f) (c*f)

mf :: Matrix -> Double -> Matrix
(Mat2 c0 c1)    `mf` f = mat2 (c0 `dot` f) (c1 `dot` f)
(Mat3 c0 c1 c2) `mf` f = mat3 (c0 `dot` f) (c1 `dot` f) (c2 `dot` f)

mv :: Matrix -> Vector -> Vector
(Mat3 (Vec3 m0 m1 m2) (Vec3 m3 m4 m5) (Vec3 m6 m7 m8)) `mv` (Vec3 v0 v1 v2) = vec3 (m0*v0+m3*v1+m6*v2) (m1*v0+m4*v1+m7*v2) (m2*v0+m5*v1*m8*v2)

mm :: Matrix -> Matrix -> Matrix
-- https://github.com/go-gl/mathgl/blob/master/mgl32/matrix.go#L1120
(Mat3 (Vec3 a0 a1 a2) (Vec3 a3 a4 a5) (Vec3 a6 a7 a8)) `mm` (Mat3 (Vec3 b0 b1 b2) (Vec3 b3 b4 b5) (Vec3 b6 b7 b8)) =
  mat3 (vec3 (a0*b0+a3*b1+a6*b2) (a1*b0+a4*b1+a7*b2) (a2*b0+a5*b1+a8*b2))
       (vec3 (a0*b3+a3*b4+a6*b5) (a1*b3+a4*b4+a7*b5) (a2*b3+a5*b4+a8*b5))
       (vec3 (a0*b6+a3*b7+a6*b8) (a1*b6+a4*b7+a7*b8) (a2*b6+a5*b7+a8*b8))

det :: Matrix -> Double
-- https://github.com/go-gl/mathgl/blob/master/mgl32/matrix.go#L1170
det (Mat2 (Vec2 aa ab) (Vec2 ba bb)) = aa*bb - ab*ba
det (Mat3 (Vec3 aa ab ac) (Vec3 ba bb bc) (Vec3 ca cb cc)) =
  aa*bb*cc + ba*cb*ac + ca*ab*bc - ca*bb*ac - ba*ab*cc - aa*cb*bc

adj :: Matrix -> Matrix
-- https://github.com/go-gl/mathgl/blob/master/mgl32/matrix.go#L1186
adj (Mat3 (Vec3 m0 m1 m2) (Vec3 m3 m4 m5) (Vec3 m6 m7 m8)) =
  mat3 (vec3 (m4*m8-m5*m7) (m2*m7-m1*m8) (m1*m5-m2*m4))
       (vec3 (m5*m6-m3*m8) (m0*m8-m2*m6) (m2*m3-m0*m5))
       (vec3 (m3*m7-m4*m6) (m1*m6-m0*m7) (m0*m4-m1*m3))

invert :: Matrix -> Matrix
invert m = (adj m) `mf` (1 / det m)

type Point = Vector

point :: Double -> Double -> Point
point = vec2

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

distance :: Point -> Double
distance (Vec2 x y) = sqrt (x**2 + y**2)
distance (Vec3 x y z) = sqrt (x**2 + y**2 + z**2)

maxnorm :: Point -> Double
maxnorm (Vec2 x y) = max (abs x) (abs y)

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
inside p (Drawing s _ t) = intersects (t `transform` p) s

onlyInside :: Point -> [Drawing] -> [Drawing]
onlyInside p d = filter (inside p) d

colorAt :: Drawing -> Point -> Color
colorAt (Drawing _ (Static c) _) _ = c
colorAt (Drawing s (Custom mapper) t) p = mapper s $ t `transform` p
colorAt d p = blend (colorAt da p) (colorAt db p)
    where Drawing s (Blended ca cb) t = d
          da = Drawing s ca t
          db = Drawing s cb t

blendedColorAt :: [Drawing] -> Point -> Color
blendedColorAt [] _ = color 0 0 0 0
blendedColorAt (d:ds) p = blend (d `colorAt` p) (ds `blendedColorAt` p)
