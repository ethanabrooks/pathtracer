module Object ( Object (..)
              , Form (..)
              , distanceFrom
              , objects
              , march
              , getNormal
              ) where

import Util (Ray (..), black, white)
import Triple (Triple (..), Vec3, RGB8, norm2, dot)
import Data.Vector (Vector, fromList)
import Control.Monad
import Debug.Trace


data Object = Object { _color      :: RGB8
                     , _light      :: Bool
                     , _reflective :: Bool
                     , _form       :: Form
                     , _name       :: String }
  

data Form = Disk
            { _center :: Vec3
            , _normal :: Vec3
            , _radius :: Double }
          | InfinitePlane
            { _normal :: Vec3
            , _point  :: Vec3 }


infPlane = Object 
  { _color      = pure 100
  , _name       = "infinite plane"
  , _light      = False
  , _reflective = False
  , _form       = InfinitePlane
             { _normal = Triple 0 0 1
             , _point  = Triple 0 0 20 }
  }

light = Object
  { _color       = pure 255
  , _name        = "light"
  , _light       = True
  , _reflective  = True
  , _form        = Disk
    { _center = Triple 0.3 0.1 0.5
    , _normal = Triple 0 1 1
    , _radius = 0.2 }
  }

disk = Object
  { _color       = pure 100
  , _name        = "disk"
  , _light       = False
  , _reflective  = False
  , _form        = Disk
    { _center = Triple 0 1 0.5
    , _normal = Triple 1 0 1
    , _radius = 0.3 }
  }

-- some_vec = pure 1

objects :: Vector Object
objects = fromList [light, infPlane] 

---
 
march :: Ray -> Double -> Vec3
march Ray{ _origin = origin, _vector = vector } distance = origin + fmap (distance *) vector

---

distanceFrom :: Ray -> Form -> Maybe Double
distanceFrom ray@Ray { _origin = origin, _vector = vector } form =
  do distance <- distanceFrom' ray form
     guard $ 0 < distance && distance < 1/0
     return distance

distanceFrom' :: Ray -> Form -> Maybe Double
distanceFrom' ray@Ray { _origin = origin, _vector = vector } form =
  case form of
    Disk { _center = center, _normal = normal, _radius = radius }  -> do
      distanceFromOrigin    <- distanceFrom ray
                               $ InfinitePlane { _point = center , _normal = normal }
      let point              = march ray distanceFromOrigin
      let distanceFromCenter = norm2 $ point - center
      guard $ distanceFromCenter < radius
      return distanceFromOrigin
    InfinitePlane { _point = point, _normal = normal } ->
        Just $ (point - origin) `dot` normal / vector `dot` normal

---

getNormal :: Form -> Vec3
getNormal form = _normal form

---

 {-
TODO:
add spheres
-}
        
