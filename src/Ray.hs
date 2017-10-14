{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Ray where

import Control.Monad
import Object (Form(..), Object)
import qualified System.Random as Random
import Triple (Triple(..), Vec3, norm2, dot, normalize)
import qualified Util
import Util (Point(..), Vector(..))

data Ray = Ray
  { _origin :: Point
  , _vector :: Vector
  , _gen :: Random.StdGen
  , _lastStruck :: Maybe Object
  }

getVector :: Ray -> Triple Double
getVector Ray {_vector = Vector vector} = vector

march :: Ray -> Double -> Vec3
march (Ray (Point origin) (Vector vector) _ _) distance =
  origin + ((distance *) <$> vector)

distanceFrom :: Ray -> Form -> Maybe Double
distanceFrom ray@Ray {_origin = origin, _vector = vector} form = do
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
      Just $ ((point - origin) `dot` normal') / (vector `dot` normal')
      where normal' = normalize normal
