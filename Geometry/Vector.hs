{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Geometry.Vector where

import Geometry.Coord
import Geometry.Matrix
import Data.Complex
import Geometry.Scalar
import Overture
import Prelude ()

newtype Vect i a = Vect { getVect :: M i One a }
    deriving (Eq, Ord, Show, Functor, Applicative)

type instance Scalar (Vect i a) = a

instance (Nat i) => Matrix (Vect i) where
    type Rows (Vect i) = i
    type Cols (Vect i) = One
    rawM = getVect
    fromRawM = Vect
    fromLists z xss = Vect $ fromLists z xss

instance (Nat i, Num a) => Num (Vect i a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

vect :: (Num a, Nat i) => [a] -> Vect i a
vect = fromLists 0 . fmap pure

vect2 :: (Num a) => [a] -> Vect Two a
vect2 = vect

getTup2 :: Vect Two a -> (a, a)
getTup2 v = let [x,y] = toList . headV . sequenceA . getM . getVect $ v
            in (x, y)

vect3 :: (Num a) => [a] -> Vect Two a
vect3 = vect

getTup3 :: Vect Three a -> (a, a, a)
getTup3 v = let [x,y,z] = toList . headV . sequenceA . getM . getVect $ v
            in (x, y, z)

vect4 :: (Num a) => [a] -> Vect Two a
vect4 = vect

complexToVect2 :: (Num a) => Complex a -> Vect Two a
complexToVect2 (r :+ i) = vect [r, i]

instance (Num a) => ToMatrix (Vect Two) (Complex a) where toMatrix = complexToVect2
instance (Num a) => FromMatrix (Vect Two) (Complex a) where 
    fromMatrix m = let [r, i] = toList . headV . sequenceA . getM . getVect $ m
                   in r :+ i


newtype CoVect j a = CoVect { getCoVect :: M One j a }
    deriving (Eq, Ord, Show, Functor, Applicative)

type instance Scalar (CoVect i a) = a

instance (Nat j) => Matrix (CoVect j) where
    type Rows (CoVect j) = One
    type Cols (CoVect j) = j
    rawM = getCoVect
    fromRawM = CoVect
    fromLists z xss = CoVect $ fromLists z xss


newtype HomogVect i a = Homog { getHomog :: Vect (S i) a }
    deriving (Eq, Ord, Show, Functor, Applicative)

type instance Scalar (HomogVect i a) = a

instance (Nat i) => Matrix (HomogVect i) where
    type Rows (HomogVect i) = (S i)
    type Cols (HomogVect i) = One
    rawM = rawM . getHomog
    fromRawM = Homog . fromRawM
    fromLists z xss = Homog $ fromLists z xss



homogInf :: (Nat i, Num a) => Vect i a -> HomogVect i a
homogInf v = Homog . Vect $ augRow (pure 0) (rawM v)

homogenize :: (Nat i, Num a) => Vect i a -> HomogVect i a
homogenize v = Homog . Vect $ augRow (pure 1) (rawM v)

homogenize' :: (Nat i, Num a) => Maybe (Vect i a) -> HomogVect i a
homogenize' Nothing  = homogInf (vect [1])
homogenize' (Just v) = homogenize v


dehomogenize :: (Nat i, Fractional a) => HomogVect i a -> Maybe (Vect i a)
dehomogenize hv = let v' = reverseV (getM $ rawM hv)
                      x = headV $ headV v'
                      xs = tailV v'
                  in if x == 0 
                     then Nothing
                     else Just $ recip x .* fromRawM (M xs)


newtype MComplex a = MComplex { getMComplex :: M Two Two a }
    deriving (Eq, Ord, Show, Functor, Applicative, Num)

type instance Scalar (MComplex a) = a

instance Matrix MComplex where
    type Rows MComplex = Two
    type Cols MComplex = Two
    rawM = getMComplex
    fromRawM = MComplex
    fromLists z xss = MComplex $ fromLists z xss

mComplex :: (Num a) => Complex a -> MComplex a
mComplex (r :+ i) = fromLists 0 [ [r, -i]
                                , [i,  r] ]

instance (Num a) => ToMatrix MComplex (Complex a) where toMatrix = mComplex


newtype ExtComplex a = ExtComplex { getExtComplex :: M Three Three a }
    deriving (Eq, Ord, Show, Functor, Applicative, Num)

type instance Scalar (ExtComplex a) = a

instance Matrix ExtComplex where
    type Rows ExtComplex = Three
    type Cols ExtComplex = Three
    rawM = getExtComplex
    fromRawM = ExtComplex
    fromLists z xss = ExtComplex $ fromLists z xss

extComplex :: (Num a) => Complex a -> ExtComplex a
extComplex (r :+ i)= fromLists 0 [ [r, -i, 0]
                                 , [i,  r, 0]
                                 , [0,  0, 1] ]

instance (Num a) => ToMatrix ExtComplex (Complex a) where toMatrix = extComplex



