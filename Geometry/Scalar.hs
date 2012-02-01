{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
module Geometry.Scalar where

import Data.Map (Map)
import Data.Set (Set)
import Data.Complex
import Prelude

type family Scalar a :: *
type instance Scalar (Complex a) = a
type instance Scalar [a] = a
type instance Scalar (Maybe a) = a
type instance Scalar (Map k v) = v
type instance Scalar (Set a) = a
