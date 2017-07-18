module Object ( Object (..)
              , Form (..)
              , distanceFrom
              , distanceFrom'
              , march
              , getNormal
              , getColor
              , objects
              ) where

import Util
import Triple
import Data.Vector (Vector, fromList)
import Control.Monad
import Debug.Trace


data Object = Object { _color      :: Vec3
                     , _emittance  :: Double
                     , _reflective :: Bool
                     , _form       :: Form
                     , _name       :: String }
  

data Form = Disk
            { _center :: Vec3
            , _normal :: Vec3
            , _radius :: Double }
          | InfinitePlane
            { _point  :: Vec3 
            , _normal :: Vec3 }
          -- | Rectangle
          --   { _center :: Vec3
          --   , _normal :: Vec3
          --   , _height :: Double
          --   , _width  :: Double }


infLight = Object 
  { _color      = pure 255
  , _name       = "infinite light"
  , _emittance  = 5
  , _reflective = True
  , _form       = InfinitePlane
             { _point  = Triple 0 0 (10)
             , _normal = Triple 1 0 (-1) }
  }

disk2 = Object
  { _color       = Triple 0 255 0
  , _name        = "disk"
  , _emittance   = 0
  , _reflective  = False
  , _form        = Disk
    { _center = Triple 0 0 10
    , _normal = Triple 1 0 (-1)
    , _radius = 600 }
  }

light = Object
  { _color       = pure 255
  , _name        = "light"
  , _emittance   = 2
  , _reflective  = True
  , _form        = Disk
    { _center = Triple 10 0 (0)
    , _normal = Triple 0 (0) (1)
    , _radius = 2 }
  }

infPlane = Object 
  { _color      = Triple 255 0 0 
  , _name       = "infinite plane"
  , _emittance  = 0
  , _reflective = False
  , _form       = InfinitePlane
                  { _point  = Triple 0 0 10
                  , _normal = Triple 1 0 (-1) }
  }

infPlane2 = Object
  { _color       = Triple 0 255 0
  , _name        = "place"
  , _emittance   = 0
  , _reflective  = False
  , _form        = InfinitePlane
                   { _point = Triple 0 0 10
                   , _normal = Triple 0 1 (-1) }
  }


objects :: Vector Object
objects = fromList [infPlane, infPlane2, light]

---
 
march :: Ray -> Double -> Vec3
march (Ray origin vector) distance = origin + fmap (distance *) vector

---

distanceFrom :: Ray -> Form -> Maybe Double
distanceFrom ray@(Ray origin vector) form =
  do distance <- distanceFrom' ray form
     guard $ 0 < distance && distance < 1/0
     return distance


distanceFrom' :: Ray -> Form -> Maybe Double
distanceFrom' ray@(Ray { _origin = origin, _vector = vector }) form =
  case form of
    Disk { _center = center, _normal = normal, _radius = radius } -> do
      distanceFromOrigin    <- distanceFrom ray $ InfinitePlane { _point = center, _normal = normal }
      let point              = march ray distanceFromOrigin
      let distanceFromCenter = norm2 $ point - center
      guard $ distanceFromCenter < radius
      return distanceFromOrigin
    -- Rectangle center normal height width -> do
    --   distanceFromOrigin    <- distanceFrom ray $ InfinitePlane center normal
    --   let point              = march ray distanceFromOrigin
    --   guard
     

      
    InfinitePlane { _normal = normal, _point = point } ->
        Just $ ((point - origin) `dot` normal) / (vector `dot` normal)

---

getNormal :: Form -> Vec3
getNormal form = _normal form

getColor :: Object -> Vec3
getColor object = _color object / 255.0

---

 {-
TODO:
add spheres
-}
        
