module Object where

import Lib (Ray (..)) 
import Triple (Vec3, RGB8, norm2, dot)
import Control.Monad
  
data Form = Disk
            { _center :: Vec3
            , _normal :: Vec3
            , _radius :: Double }
          | InfinitePlane
            { _normal :: Vec3
            , _point  :: Vec3 }
 
data Object = Object { _color      :: RGB8
                     , _light      :: Bool
                     , _reflective :: Bool
                     , _form       :: Form }

march :: Ray -> Double -> Vec3
march Ray{ _origin = origin, _vector = vector } distance = origin + fmap (distance *) vector

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


distanceFrom :: Ray -> Form -> Maybe Double
distanceFrom ray@Ray { _origin = origin, _vector = vector } form =
  do distance <- distanceFrom' ray form
     guard $ distance > 0 
     return distance

 {-
TODO:
refactor into separate files
write tests
add diffuse reflectivity
add spheres
-}
        
