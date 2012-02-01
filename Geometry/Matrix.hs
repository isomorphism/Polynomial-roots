{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Geometry.Matrix where

import Data.Complex
import Geometry.Coord
import Geometry.Scalar
import Overture
import Prelude ()

class ( Functor m, Applicative m
      , Nat (Rows m), Nat (Cols m)
      ) => Matrix m where
    type Rows m
    type Cols m
    rawM :: m a -> M (Rows m) (Cols m) a
    fromRawM :: M (Rows m) (Cols m) a -> m a
    fromLists :: a -> [[a]] -> m a

class (Matrix m) => ToMatrix m a where
    toMatrix :: a -> m (Scalar a)

class (Matrix m) => FromMatrix m a where
    fromMatrix :: m (Scalar a) -> a


newtype M i j a = M { getM :: V i (V j a) }
    deriving (Eq, Ord, Show)

type instance Scalar (M i j a) = a

instance (Nat i, Nat j) => Matrix (M i j) where
    type Rows (M i j) = i
    type Cols (M i j) = j
    rawM = id
    fromRawM = id
    fromLists z xss = M (fromList nat (pure z) $ fromList nat z <$> xss)

instance (Nat i, Nat j) => ToMatrix (M i j) (M i j a) where toMatrix = id
instance (Nat i, Nat j) => FromMatrix (M i j) (M i j a) where fromMatrix = id

instance Functor (M i j) where
    fmap f (M xss) = M $ (fmap . fmap) f xss

instance (Nat i, Nat j) => Applicative (M i j) where
    pure = M . pure . pure
    M xss <*> M yss = M $ (<*>) <$> xss <*> yss

instance (Nat i, Num a) => Num (M i i a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = multM
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger x = M $ fromList nat (pure 0) vs
      where x' = fromInteger x
            vs = fromList nat 0 . (++ [x']) <$> ((flip replicate 0 <$> [0..]))

multM :: (Nat i, Nat j, Nat k, Num a) => M i j a -> M j k a -> M i k a
multM (M xss) (M yss) = M $ (\xs -> (`dot` xs) <$> sequenceA yss) <$> xss

infixl 7 |*|
x |*| y = multM x y

infixl 6 |+|
x |+| y = liftA2 (+)

augRow :: (Nat i, Nat j) => V j a -> M i j a -> M (S i) j a
augRow xs (M xss) = M $ snocV xs xss

augCol :: (Nat i, Nat j) => V i a -> M i j a -> M i (S j) a
augCol xs (M xss) = M $ snocV <$> xs <*> xss

toRow :: (Nat i) => V i a -> M One i a
toRow v = M (singleV v)

fromRow :: (Nat i) => M One i a -> V i a
fromRow (M xss) = headV xss

toCol :: (Nat i) => V i a -> M i One a
toCol v = M (singleV <$> v)

fromCol :: (Nat i) => M i One a -> V i a
fromCol (M xss) = headV $ sequenceA xss

