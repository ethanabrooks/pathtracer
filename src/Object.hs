{-# LANGUAGE Strict #-}

module Object
  ( Object(..)
  , Form(..)
  , Ray(..)
  , Point(..)
  , Vector(..)
  , Color(..)
  , distanceFrom
  , distanceFrom'
  , march
  , getNormal
  , getVector
  , getColor
  , objects
  ) where

import Control.Monad
import qualified Data.Vector as V
import qualified System.Random as Random
import Triple (Triple(..), Vec3, norm2, dot, normalize)

data Color =
  Color (Triple Double)

newColor a b c = Color $ Triple a b c

instance Eq Color where
  Color c1 == Color c2 = c1 == c2

data Object = Object
  { _color :: Color
  , _emittance :: Double
  , _reflective :: Bool
  , _form :: Form
  , _name :: String
  }

instance Eq Object where
  Object c1 e1 r1 f1 n1 == Object c2 e2 r2 f2 n2 =
    c1 == c2 && e1 == e2 && r1 == r2 && f1 == f2 && n1 == n2

data Point =
  Point (Triple Double)

instance Eq Point where
  Point p1 == Point p2 = p1 == p2

instance Eq Vector where
  Vector v1 == Vector v2 = v1 == v2

data Form
  = Disk { _center :: Point
        ,  _normal :: Vector
        ,  _radius :: Double}
  | InfinitePlane { _point :: Point
                 ,  _normal :: Vector}
          -- | Rectangle
          --   { _center :: Vec3
          --   , _normal :: Vec3
          --   , _height :: Double
          --   , _width  :: Double }

instance Eq Form where
  Disk c1 n1 r1 == Disk c2 n2 r2 = c1 == c2 && n1 == n2 && r1 == r2
  InfinitePlane p1 n1 == InfinitePlane p2 n2 = p1 == p2 && n1 == n2
  _ == _ = False

data Vector =
  Vector (Triple Double)

newPoint a b c = Point (Triple a b c)

newVector a b c = Vector (Triple a b c)

data Ray = Ray
  { _origin :: Point
  , _vector :: Vector
  , _gen :: Random.StdGen
  , _lastStruck :: Maybe Object
  }

infLight =
  Object
  { _color = Color $ pure 1
  , _name = "infinite light"
  , _emittance = 5
  , _reflective = True
  , _form =
      InfinitePlane
      {_point = Point $ Triple 0 0 (-10), _normal = Vector $ Triple 0 0 (-1)}
  }

disk =
  Object
  { _color = Color $ Triple 0 1 0
  , _name = "disk"
  , _emittance = 0
  , _reflective = False
  , _form =
      Disk
      {_center = newPoint 0 0 10, _normal = newVector 1 0 (-1), _radius = 600}
  }

light =
  Object
  { _color = Color $ pure 1
  , _name = "light"
  , _emittance = 2
  , _reflective = True
  , _form =
      Disk
      {_center = newPoint 0 0 (-1), _normal = newVector 0 (0) (1), _radius = 20}
  }

infPlane =
  Object
  { _color = Color . pure $ 1
  , _name = "plane 1"
  , _emittance = 0
  , _reflective = False
  , _form =
      InfinitePlane
      {_point = newPoint 20 (-20) 100, _normal = newVector (0) 0 (-1)}
  }

infPlane2 =
  Object
  { _color = newColor 0.1 1 1
  , _name = "plane 2"
  , _emittance = 0
  , _reflective = False
  , _form =
      InfinitePlane {_point = newPoint 20 (-20) 100, _normal = newVector 0 1 0}
  }

infPlane3 =
  Object
  { _color = newColor 1 1 0.1
  , _name = "plane 3"
  , _emittance = 0
  , _reflective = False
  , _form =
      InfinitePlane
      {_point = newPoint 20 (-20) 100, _normal = newVector (-1) 0 0}
  }

objects :: V.Vector Object
objects = V.fromList [infPlane, infPlane2, infPlane3, light]

---
march :: Ray -> Double -> Vec3
march (Ray (Point origin) (Vector vector) _ _) distance =
  origin + ((distance *) <$> vector)

---
distanceFrom :: Ray -> Form -> Maybe Double
distanceFrom ray@(Ray {_origin = origin, _vector = vector}) form = do
  distance <- distanceFrom' ray form
  guard $ 0 < distance && distance < 1 / 0
  return distance

distanceFrom' :: Ray -> Form -> Maybe Double
distanceFrom' ray@(Ray (Point origin) (Vector vector) _ _) form =
  case form of
    Disk (Point center) (Vector normal) radius -> do
      distanceFromOrigin <-
        distanceFrom ray $ InfinitePlane (Point center) (Vector normal)
      let point = march ray distanceFromOrigin
      let distanceFromCenter = norm2 $ point - center
      guard $ distanceFromCenter < radius
      return distanceFromOrigin
    -- Rectangle center normal height width -> do
    --   distanceFromOrigin    <- distanceFrom ray $ InfinitePlane center normal
    --   let point              = march ray distanceFromOrigin
    --   guard
    InfinitePlane (Point point) (Vector normal) ->
      Just $ ((point - origin) `dot` normal) / (vector `dot` normalize normal)

---
getNormal :: Form -> Triple Double
getNormal form = normal
  where
    Vector normal = _normal form

getColor :: Object -> Triple Double
getColor Object {_color = Color color} = color

getVector :: Ray -> Triple Double
getVector Ray {_vector = Vector vector} = vector
---
 {-
TODO:
add spheres
-}
