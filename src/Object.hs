{-# LANGUAGE Strict #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Object where

import Color (Color, colorToTriple)
import Triple (Triple, Vec3)
import Util (Point(..), Vector(..))

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
  deriving (Eq)

{-
instance Eq Form where
  Disk c1 n1 r1 == Disk c2 n2 r2 = c1 == c2 && n1 == n2 && r1 == r2
  InfinitePlane p1 n1 == InfinitePlane p2 n2 = p1 == p2 && n1 == n2
  _ == _ = False
  -}
data Object = Object
  { _color :: Color Double
  , _emittance :: Double
  , _reflective :: Bool
  , _form :: Form
  , _name :: String
  } deriving (Eq)

{-
instance Eq Object where
  Object c1 e1 r1 f1 n1 == Object c2 e2 r2 f2 n2 =
    c1 == c2 && e1 == e2 && r1 == r2 && f1 == f2 && n1 == n2
    -}
---
---
---
getNormal :: Form -> Triple Double
getNormal form = normal
  where
    Vector normal = _normal form

getColor :: Object -> Color Double
getColor Object {_color = color} = color
 {-
TODO:
add spheres
-}
---
{-
instance Elt Ray where 
  eltType _ = eltType  (undefined :: (Vec3, Vec3, Int, Maybe Int)
  toElt p = let (point, vector, seed, lastStruck) = toElt p
             in Ray point vector seed 
             -}
