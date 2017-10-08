module Objects where

import qualified Data.Vector as V
import Object (Object(..), Form(..))
import Triple (Triple(..))
import Util (Color, Vector, Point, newPoint, newVector, newColor)
import qualified Util

infLight =
  Object
  { _color = Util.Color $ pure 1
  , _name = "infinite light"
  , _emittance = 5
  , _reflective = True
  , _form =
      InfinitePlane
      { _point = Util.Point $ Triple 0 0 (-10)
      , _normal = Util.Vector $ Triple 0 0 (-1)
      }
  }

disk =
  Object
  { _color = Util.Color $ Triple 0 1 0
  , _name = "disk"
  , _emittance = 0
  , _reflective = False
  , _form =
      Disk
      {_center = newPoint 0 0 10, _normal = newVector 1 0 (-1), _radius = 600}
  }

light =
  Object
  { _color = Util.Color $ pure 1
  , _name = "light"
  , _emittance = 2
  , _reflective = True
  , _form =
      Disk
      {_center = newPoint 0 0 (-1), _normal = newVector 0 0 1, _radius = 20}
  }

infPlane =
  Object
  { _color = Util.Color . pure $ 1
  , _name = "plane 1"
  , _emittance = 0
  , _reflective = False
  , _form =
      InfinitePlane
      {_point = newPoint 20 (-20) 100, _normal = newVector 0 0 (-1)}
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
