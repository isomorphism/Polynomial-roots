{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Geometry.Transform where

import Interval
import Geometry.Scalar
import Geometry.Vector
import Geometry.Matrix
import Geometry.Coord
import Data.Monoid
import Overture
import Prelude ()

-- transformations with approximate inverses, instances should behave sensibly
--   i.e. forward transformation is surjective, backwards is injective, and
--        restricting the source to the image of the backwards transformation
--        gives the obvious bijection (modulo numerical errors I guess)
class Transform t where
    type Source t
    type Dest t
    (<@) :: t -> Source t -> Dest t
    (@>) :: t -> Dest t -> Source t

infixr 9 <@, @>

data a :~> b = Transform (a -> b) (b -> a)

instance Category (:~>) where
    id = Transform id id
    Transform g g' . Transform f f' = Transform (g . f) (f' . g')

instance Transform (a :~> b) where
    type Source (a :~> b) = a
    type Dest (a :~> b) = b
    (Transform f _ ) <@ x = f x
    (Transform _ f') @> x = f' x

infixr 1 <~~, ~~>
g <~~ f = forget g <<< forget f
f ~~> g = forget f >>> forget g

forget :: (Transform t) => t -> (Source t :~> Dest t)
forget t = Transform (t <@) (t @>)

asHomog :: (Fractional a, Nat i) => Maybe (Vect i a) :~> HomogVect i a
asHomog = Transform homogenize' dehomogenize

viaHomog :: (Fractional a, Nat i) => HomogVect i a :~> Maybe (Vect i a)
viaHomog = Transform dehomogenize homogenize'

data Offset a = Offset a

instance (Num a) => Monoid (Offset a) where
    mempty = Offset 0
    mappend (Offset x) (Offset y) = Offset (x + y)

instance (Num a) => Transform (Offset a) where
    type Source (Offset a) = a
    type Dest (Offset a) = a
    Offset o <@ x = o + x
    Offset o @> x = x - o

data Linear i a = Linear (M i i a) (M i i a)
    deriving (Eq, Ord, Show)

instance (Nat i, Num a) => Monoid (Linear i a) where
    mempty = Linear 1 1
    mappend (Linear x x') (Linear y y') = Linear (x |*| y) (y' |*| x')

instance (Nat i, Num a) => Transform (Linear i a) where
    type Source (Linear i a) = Vect i a
    type Dest (Linear i a) = Vect i a
    (<@) = applyLin
    (@>) = applyInvLin
    

applyLin :: (Num a, Nat i) => Linear i a -> Vect i a -> Vect i a
applyLin (Linear m _) (Vect v) = Vect $ m |*| v

applyInvLin :: (Num a, Nat i) => Linear i a -> Vect i a -> Vect i a
applyInvLin (Linear _ m) (Vect v) = Vect $ m |*| v

-- rotate clockwise by fraction of a complete revolution
rotate2d :: (Floating a) => a -> Linear Two a
rotate2d r = Linear (fromLists 0 [ [  cos θ, - sin θ ]
                                 , [  sin θ,   cos θ ] ])
                    (fromLists 0 [ [  cos θ,   sin θ ]
                                 , [- sin θ,   cos θ ] ])
  where θ = r * 2 * pi

scale :: (Floating a, Nat i) => a -> Linear i a
scale s = Linear (s .* 1) (recip s .* 1)

stretch2d :: (Floating a) => a -> a -> Linear Two a
stretch2d x y = Linear (fromLists 0 [[x, 0], [0, y]])
                       (fromLists 0 [[recip x, 0], [0, recip y]])


data Affine i a = Affine (M (S i) (S i) a) (M (S i) (S i) a)
    deriving (Eq, Ord, Show)

instance (Nat i, Num a) => Monoid (Affine i a) where
    mempty = Affine 1 1
    mappend (Affine x x') (Affine y y') = Affine (x |*| y) (y' |*| x')

instance (Nat i, Num a) => Transform (Affine i a) where
    type Source (Affine i a) = HomogVect i a
    type Dest (Affine i a) = HomogVect i a
    (<@) = applyAff
    (@>) = applyInvAff

applyAff :: (Num a, Nat i) => Affine i a -> HomogVect i a -> HomogVect i a
applyAff (Affine m _) (Homog (Vect v)) = Homog . Vect $ m |*|  v

applyInvAff :: (Num a, Nat i) => Affine i a -> HomogVect i a -> HomogVect i a
applyInvAff (Affine _ m) (Homog (Vect v)) = Homog . Vect $ m |*| v

linToAff :: (Num a, Nat i) => Linear i a -> Affine i a
linToAff (Linear l l') = Affine (aug l) (aug l')
  where aug = augCol (snocV 1 $ pure 0) . augRow (pure 0)

translate :: (Nat i, Num a) => HomogVect i a -> Affine i a
translate (Homog (Vect v)) = Affine (augCol (fromCol v). augRow (pure 0) $ 1)
                                    (augCol (fromCol $ negate <$> v) . augRow (pure 0) $ 1)

rotateAff2d :: (Floating a) => a -> Affine Two a
rotateAff2d = linToAff . rotate2d

scaleAff :: (Floating a, Nat i) => a -> Affine i a
scaleAff = linToAff . scale

stretchAff2d :: (Floating a) => a -> a -> Affine Two a
stretchAff2d x y = linToAff $ stretch2d x y

data Mobius a = Mobius (ExtComplex a) (ExtComplex a)
    deriving (Eq, Ord, Show)

instance (Num a) => Monoid (Mobius a) where
    mempty = Mobius 1 1
    mappend (Mobius x x') (Mobius y y') = Mobius (x * y) (y' * x')

instance (Num a) => Transform (Mobius a) where
    type Source (Mobius a) = ExtComplex a
    type Dest (Mobius a) = ExtComplex a
    (<@) = applyMobius
    (@>) = applyInvMobius

applyMobius :: (Num a) => Mobius a -> ExtComplex a -> ExtComplex a
applyMobius (Mobius m _) c = m * c

applyInvMobius :: (Num a) => Mobius a -> ExtComplex a -> ExtComplex a
applyInvMobius (Mobius _ m) c = m * c



data Discrete f a b = Discrete (f a -> f b) (f b -> f a)

instance (RealFrac a, Integral b) => Transform (Discrete m a b) where
    type Source (Discrete m a b) = m a
    type Dest (Discrete m a b) = m b
    Discrete f _  <@ x = f x
    Discrete _ f' @> x = f' x

rounding :: (Functor f, Integral b, RealFrac a) => Discrete f a b
rounding = Discrete (fmap round) (fmap fromIntegral)



data Iso (~>) a b = Iso (a ~> b) (b ~> a)
type a :<->: b = Iso (->) a b

instance Transform (Iso (->) a b) where
    type Source (Iso (->) a b) = a
    type Dest (Iso (->) a b) = b
    Iso f _  <@ x = f x
    Iso _ f' @> x = f' x

instance (Category ar) => Category (Iso ar) where
    id = Iso id id
    Iso f g . Iso f' g' = Iso (f . f') (g' . g)

flipIso :: Iso (~>) a b -> Iso (~>) b a
flipIso (Iso f g) = Iso g f

integrals :: (Integral a, Integral b) => a :<->: b
integrals = Iso fromIntegral fromIntegral

realfracs :: (RealFrac a, RealFrac b) => a :<->: b
realfracs = Iso realToFrac realToFrac

fromM :: (Rows m ~ i, Cols m ~ j, Matrix m) => M i j a :<->: m a
fromM = Iso fromRawM rawM

toM :: (Rows m ~ i, Cols m ~ j, Matrix m) => m a :<->: M i j a
toM = flipIso fromM

convM :: (Matrix m, Matrix n, Cols m ~ Cols n, Rows m ~ Rows n) => m a :<->: n a
convM = fromM . toM

asMatrix :: (ToMatrix m a, FromMatrix m a) => a :<->: m (Scalar a)
asMatrix = Iso toMatrix fromMatrix

viaMatrix :: (ToMatrix m a, FromMatrix m a) => m (Scalar a) :<->: a
viaMatrix = flipIso asMatrix

