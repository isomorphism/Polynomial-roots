{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Geometry.Coord where

import Data.Label.Pure
import Geometry.Scalar
import Overture
import Prelude ()

data S n = S n | Zero
data Z = Z

class Nat n where
    nat :: n
    fromN :: n -> Integer
    toN :: Integer -> Maybe n
    replicateN :: n -> a -> V n a
    iterN :: n -> (a -> a) -> a -> V n a
    fromList :: n -> a -> [a] -> V n a
    atIndexN :: n -> V (S n) a -> a

instance Nat Z where
    nat = Z
    fromN _ = 0
    toN 0 = Just Z
    toN _ = Nothing
    replicateN _ _ = Nil
    iterN _ _ _ = Nil
    fromList _ _ _ = Nil
    atIndexN _ (x :. Nil) = x

instance (Nat n) => Nat (S n) where
    nat = S nat
    fromN (S n) = 1 + fromN n
    toN 0 = Just Zero
    toN n | n > 0     = S <$> toN (n - 1)
          | otherwise = Nothing
    replicateN n x = x :. replicateN (predN n) x
    iterN n f z = z :. iterN (predN n) f (f z)
    fromList n z [] = replicateN nat z
    fromList n z (x:xs) = x :. fromList (predN n) z xs
    atIndexN Zero (x :. _) = x
    atIndexN (S n) (_ :. xs) = atIndexN n xs

predN :: (Nat n) => (S n) -> n
predN _ = nat


type One = S Z
one :: One
one = S Z

type Two = S One
two :: Two
two = S one

type Three = S Two
three :: Three
three = S two

type Four = S Three
four :: Four
four = S three


infixr 8 :.
data V n a where
    Nil  :: V Z a
    (:.) :: (Nat n) => a -> V n a -> V (S n) a

type instance Scalar (V i a) = a

zipV :: V n a -> V n b -> V n (a, b)
zipV Nil Nil = Nil
zipV (x :. xs) (y :. ys) = (x, y) :. zipV xs ys

zipWithV :: (a -> b -> c) -> V n a -> V n b -> V n c
zipWithV _ Nil Nil = Nil
zipWithV f (x :. xs) (y :. ys) = f x y :. zipWithV f xs ys

zapV :: V n (a -> b) -> V n a -> V n b
zapV Nil Nil = Nil
zapV (f :. fs) (x :. xs) = f x :. (zapV fs xs)


headV :: (Nat n) => V (S n) a -> a
headV (x :. _) = x

tailV :: (Nat n) => V (S n) a -> V n a
tailV (_ :. xs) = xs

consV :: (Nat n) => a -> V n a -> V (S n) a
consV x xs = x :. xs

snocV :: (Nat n) => a -> V n a -> V (S n) a
snocV z Nil = z :. Nil
snocV z (x :. xs) = x :. snocV z xs

reverseV :: (Nat n) => V n a -> V n a
reverseV Nil = Nil
reverseV (x :. xs) = snocV x $ reverseV xs

initV :: (Nat n) => V (S n) a -> V n a
initV = reverseV . tailV . reverseV

lastV :: (Nat n) => V (S n) a -> a
lastV = headV . reverseV

singleV :: a -> V One a
singleV x = x :. Nil

instance Functor (V n) where
    fmap _ Nil = Nil
    fmap f (x :. xs) = f x :. fmap f xs

instance (Nat n) => Applicative (V n) where
    pure x = replicateN nat x
    Nil <*> Nil = Nil
    f :. fs <*> x :. xs = f x :. (fs <*> xs)

instance Foldable (V n) where
    foldr f z Nil = z
    foldr f z (x :. xs) = f x (foldr f z xs)

instance Traversable (V n) where
    sequenceA Nil = pure Nil
    sequenceA (x :. xs) = (:.) <$> x <*> sequenceA xs

instance (Eq a) => Eq (V n a) where
    x == y = and $ zipWithV (==) x y

instance (Ord a) => Ord (V n a) where
    compare x y = compare (toList x) (toList y)

instance (Show a) => Show (V n a) where
    show = show . toList 

instance (Nat n, Num a) => Num (V n a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    negate = fmap negate
    fromInteger = pure . fromInteger

instance (Nat n, Fractional a) => Fractional (V n a) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = pure . fromRational


dot v1 v2 = sum $ (*) <$> v1 <*> v2


(.*) :: (Functor f, Num a) => a -> f a -> f a
x .* xs = fmap (x *) xs

(*.) :: (Functor f, Num a) => f a -> a -> f a
xs *. x = fmap (* x) xs



