{-# LANGUAGE DataKinds #-}
module Math(
  Vector(Vec2, Vec3), Matrix(Mat2, Mat3),

  interpolate, sample,

  vec2, vec3, getX, getY,
  mat2, mat3, ident2, ident3, mat2to3, transpose,
  dot, mf, mv, mm, det, adj, invert,
  ) where


-- Generate an evenly spaced sample between two values (at the given position)
-- e.g. sample -1.5 +1.5 25 2 = -1.25
interpolate :: Double -> Double -> Int -> Int -> Double
interpolate c0 c1 n i = c0 + ((c1-c0) / (fromIntegral $ n-1) * fromIntegral i)

sample :: Int -> Int -> Double -> Int
sample a b d = (floor $ d * fromIntegral (b - a)) + a

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
