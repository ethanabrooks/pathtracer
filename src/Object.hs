module Object ( Object (..)
              , Form (..)
              , distanceFrom
              , objects
              , march
              , getNormal
              ) where

import Lib (Ray (..), black, white) 
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
  { _color      = black
  , _name       = "infinite plane"
  , _light      = False
  , _reflective = True
  , _form       = InfinitePlane
             { _normal = some_vec
             , _point  = some_vec }
  }

light = Object
  { _color       = white
  , _name        = "light"
  , _light       = True
  , _reflective  = True
  , _form        = Disk
    { _center = Triple 1 1 0.5
    , _normal = some_vec
    , _radius = 1 }
  }

some_vec = pure 1

objects :: Vector Object
objects = fromList [light] 

---
 
march :: Ray -> Double -> Vec3
march Ray{ _origin = origin, _vector = vector } distance = origin + fmap (distance *) vector

---

distanceFrom :: Ray -> Form -> Maybe Double
distanceFrom ray@Ray { _origin = origin, _vector = vector } form =
  do distance <- traceShowId $ distanceFrom' ray form
     guard $ 0 < distance && distance < 1/0
     return distance

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
write tests
add diffuse reflectivity
add spheres
-}
        
