module Object ( Object (..)
              , Form (..)
              , distanceFrom
              , distanceFrom'
              , march
              , getNormal
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
            { _normal :: Vec3
            , _point  :: Vec3 }
          -- | Rectangle
          --   { _center :: Vec3
          --   , _normal :: Vec3
          --   , _height :: Double
          --   , _width  :: Double }


infPlane = Object 
  { _color      = Triple 255 0 0 
  , _name       = "infinite plane"
  , _emittance  = 0
  , _reflective = False
  , _form       = InfinitePlane
             { _normal = Triple 0 0 (-1)
             , _point  = Triple 0 0 0 }
  }

infLight = Object 
  { _color      = pure 255
  , _name       = "infinite light"
  , _emittance  = 5
  , _reflective = True
  , _form       = InfinitePlane
             { _normal = Triple 0 0 1
             , _point  = Triple 0 0 (-10) }
  }

light = Object
  { _color       = pure 255
  , _name        = "light"
  , _emittance   = 2
  , _reflective  = True
  , _form        = Disk
    { _center = Triple (10) (30) (-20)
    , _normal = Triple 0 (0) (1)
    , _radius = 10 }
  }

disk1 = Object
  { _color       = Triple 55 100 255
  , _name        = "disk"
  , _emittance   = 0
  , _reflective  = False
  , _form        = Disk
    { _center = Triple 0 0 0
    , _normal = Triple 0 0 (-1)
    , _radius = 30 }
  }

disk2 = Object
  { _color       = Triple 0 255 0
  , _name        = "disk"
  , _emittance   = 0
  , _reflective  = False
  , _form        = Disk
    { _center = Triple 0 0 0
    , _normal = Triple 0 0 (-1)
    , _radius = 600 }
  }


objects :: Vector Object
objects = fromList [light, disk1]

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
        
