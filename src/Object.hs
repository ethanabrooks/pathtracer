module Object ( Object (..)
              , Form (..)
              , distanceFrom
              , distanceFrom'
              , objects
              , march
              , getNormal
              ) where

import Util
import Triple
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
          -- | Rectangle
          --   { _center :: Vec3
          --   , _normal :: Vec3
          --   , _height :: Double
          --   , _width  :: Double }
          | InfinitePlane
            { _normal :: Vec3
            , _point  :: Vec3 }


infPlane = Object 
  { _color      = pure 100
  , _name       = "infinite plane"
  , _light      = False
  , _reflective = True
  , _form       = InfinitePlane
             { _normal = Triple 0 0 (-1)
             , _point  = Triple 0 0 10 }
  }

infLight = Object 
  { _color      = Triple 255 0 0
  , _name       = "infinite light"
  , _light      = True
  , _reflective = True
  , _form       = InfinitePlane
             { _normal = Triple 0 0 1
             , _point  = Triple 0 0 (-10) }
  }

light = Object
  { _color       = pure 255
  , _name        = "light"
  , _light       = True
  , _reflective  = True
  , _form        = Disk
    { _center = Triple 0 0 (10)
    , _normal = Triple 0 0 (-1)
    , _radius = 2000 }
  }

disk = Object
  { _color       = pure 100
  , _name        = "disk"
  , _light       = False
  , _reflective  = False
  , _form        = Disk
    { _center = Triple 5 1 0.5
    , _normal = Triple 1 0 1
    , _radius = 0.3 }
  }

-- some_vec = pure 1

objects :: Vector Object
objects = fromList [light] 

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
distanceFrom' ray@(Ray origin vector) form =
  case form of
    Disk center normal radius -> do
      distanceFromOrigin    <- distanceFrom ray $ InfinitePlane normal center
      let point              = march ray distanceFromOrigin
      let distanceFromCenter = norm2 $ point - center
      guard $ distanceFromCenter < radius
      return distanceFromOrigin
    -- Rectangle center normal height width -> do
    --   distanceFromOrigin    <- distanceFrom ray $ InfinitePlane center normal
    --   let point              = march ray distanceFromOrigin
    --   guard
     

      
    InfinitePlane normal point ->
        Just $ ((point - origin) `dot` normal) / (vector `dot` normal)

        -- Just $ (((traceShowId $ trace "point" point) - (traceShowId $ trace "origin" origin)) `dot` normal) / ((traceShowId $ trace "vector" vector) `dot` (traceShowId $ trace "normal" normal))

---

getNormal :: Form -> Vec3
getNormal form = _normal form

---

 {-
TODO:
add spheres
-}
        
