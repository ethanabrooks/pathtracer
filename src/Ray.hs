module Ray where

import Control.Monad
import Object (Form(..))
import qualified System.Random as Random
import Triple (Triple(..), Vec3, norm2, dot, normalize)
import Util (Point, Vector)
import qualified Util

data Ray = Ray
  { _origin :: Util.Point
  , _vector :: Util.Vector
  , _gen :: Random.StdGen
  , _lastStruck :: Maybe Int
  } deriving (Show)

getVector :: Ray -> Triple Double
getVector Ray {_vector = Util.Vector vector} = vector

march :: Ray -> Double -> Vec3
march (Ray (Util.Point origin) (Util.Vector vector) _ _) distance =
  origin + ((distance *) <$> vector)

distanceFrom :: Ray -> Form -> Maybe Double
distanceFrom ray@Ray {_origin = origin, _vector = vector} form = do
  distance <- distanceFrom' ray form
  guard $ 0 < distance && distance < 1 / 0
  return distance

distanceFrom' :: Ray -> Form -> Maybe Double
distanceFrom' ray@(Ray (Util.Point origin) (Util.Vector vector) _ _) form =
  case form of
    Disk (Util.Point center) (Util.Vector normal) radius -> do
      distanceFromOrigin <-
        distanceFrom ray $
        InfinitePlane (Util.Point center) (Util.Vector normal)
      let point = march ray distanceFromOrigin
      let distanceFromCenter = norm2 $ point - center
      guard $ distanceFromCenter < radius
      return distanceFromOrigin
    -- Rectangle center normal height width -> do
    --   distanceFromOrigin    <- distanceFrom ray $ InfinitePlane center normal
    --   let point              = march ray distanceFromOrigin
    --   guard
    InfinitePlane (Util.Point point) (Util.Vector normal) ->
      Just $ ((point - origin) `dot` normal) / (vector `dot` normalize normal)
