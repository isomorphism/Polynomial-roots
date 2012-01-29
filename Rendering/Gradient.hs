{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Rendering.Gradient where

import Control.Arrow
import Control.Applicative
import Data.Colour.Names
import Data.List
import Rendering.Colour
import Types hiding(Gradient)
import Util

newtype Gradient clr a = Grad { runGrad :: Maybe Double -> clr a }

colourAt :: Gradient AlphaColour Double -> Maybe Double -> RGBAColour
colourAt = runGrad

apGrad :: Maybe Double -> Gradient f a -> f a
apGrad = flip runGrad

instance (AffineSpace f) => AffineSpace (Gradient f) where
    affineCombo xs z = Grad $ \n -> calcGrad n
      where calcGrad n = affineCombo (second (apGrad n) <$> xs) (apGrad n z)

instance (ColourOps f) => ColourOps (Gradient f) where
    over c (Grad f) = Grad $ \n -> over c (f n)
    darken s (Grad f) = Grad $ \n -> darken s (f n)

instance (Monoid (f a)) => Monoid (Gradient f a) where
    mempty = constant unit
    mappend (Grad g1) (Grad g2) = Grad $ \n -> g1 n <> g2 n


constant :: f a -> Gradient f a
constant = Grad . const

linear :: (Monoid (f a), Fractional a, AffineSpace f) => f a -> f a -> Gradient f a
linear c1 c2 = Grad $ maybe unit (\n -> blend (realToFrac n) c1 c2)

onInput :: (Double -> Double) -> Gradient f a -> Gradient f a
onInput f = Grad . (. fmap f) . runGrad

onOutput :: (f a -> g a) -> Gradient f a -> Gradient g a
onOutput f = Grad . (f .) . runGrad

invert, square, squareRoot :: Gradient f a -> Gradient f a
invert = onInput recip
square = onInput (^ 2)
squareRoot = onInput sqrt

adjacent :: Double -> f a -> Gradient f a -> Gradient f a -> Gradient f a
adjacent x z g1 g2 = Grad $ maybe z (\n -> runGrad (if n < x then g1 else g2) $ Just n)

--Takes a list of colours and control points, giving the corresponding gradient.
collate :: (Monoid (f a), Ord a, Fractional a, AffineSpace f) => [(f a, a)] -> Gradient f a
collate cvs1 = Grad $ maybe unit (blender cvs')
    where cvs2 = sortBy (\a b -> compare (snd a) (snd b)) cvs1
          cvs3 = filter (\(_,b) -> (b >= 0 && b <= 1)) cvs2
          cvs' = case (head cvs3, last cvs3) of
                      ((_,0),(_,1)) -> cvs3
                      ((_,0),(b,_)) -> cvs3 ++ [(b,1)]
                      ((a,_),(_,1)) -> [(a,0)] ++ cvs3
                      ((a,_),(b,_)) -> [(a,0)] ++ cvs3 ++ [(b,1)]
          blender cvs n = blend n' c1 c2
              where n2 = min 1 . max 0 $ realToFrac n
                    (c1,a1) = last $ takeWhile (\(a,b) -> b <= n2) cvs
                    (c2,a2) = head $ dropWhile (\(a,b) -> b < n2) cvs
                    n' = (n2-a1)/(a2-a1)

asHue :: Gradient AlphaColour Double
asHue = Grad $ maybe transparent (\n -> opaque $ hsv n 1 1)

opacify :: (Num a) => Colour a -> Gradient AlphaColour a -> Gradient Colour a
opacify bg = onOutput (`over` bg)

fadeIn c = linear (opaque c) transparent
fadeOut c = linear transparent (opaque c)

warm = collate [(opaque black,0),(opaque red,1/3),(opaque yellow,2/3),(opaque white,1)]
cold = collate [(opaque black,0),(opaque blue,1/3),(opaque cyan,2/3),(opaque white,1)]
sunset = collate [(opaque black,0),(opaque purple, 1/5), (opaque orange, 1/2), (opaque white, 1)]

monochrome = fadeIn white

gradientByName "warm" = Just warm
gradientByName "cold" = Just cold
gradientByName "sunset" = Just sunset
gradientByName "monochrome" = Just monochrome
gradientByName "transparent" = Just $ constant transparent
gradientByName _ = Nothing


gradientFromSpec :: Gradient AlphaColour Double -> Colour Double 
                 -> GradientSpec -> Gradient Colour Double 
gradientFromSpec def bg gSpec = opacify bg $ fromExpr gSpec ?? def

-- fromExpr :: GradientSpec -> Maybe (Gradient AlphaColour Double)
fromExpr (NamedGradient g) = gradientByName g
fromExpr (Split gns g) = splitGrads gns g
fromExpr (Combine f xs) = combineGrads (getBlendFunc f) =<< mapM fromExpr xs
fromExpr (Transform f g) = getTransFunc f <$> fromExpr g


-- combineGrads :: (Gradient AlphaColour a -> Gradient AlphaColour a -> Gradient AlphaColour a) 
--             -> [Gradient AlphaColour a] -> Maybe (Gradient AlphaColour a)
combineGrads f [] = Nothing
combineGrads f [x] = Just x
combineGrads f (x:xs) = Just $ foldl f x xs

splitGrads [] g = fromExpr g
splitGrads ((g1, n1):gns) g = adjacent n1 transparent <$> g1' <*> splitGrads gns g
  where g1' = fromExpr g1

-- getBlendFunc :: BlendFunction -> Gradient AlphaColour Double 
--              -> Gradient AlphaColour Double -> Gradient AlphaColour Double
getBlendFunc Blend = blend 0.5
getBlendFunc Overlay = (<>)

getTransFunc Invert = invert
getTransFunc (Exponent n) = onInput (** n)
getTransFunc Reverse = onInput (1 -)

--------------------------------------------------------------------------------
--Converting polynomials to values to be able to apply gradients.

--Converts a polynomials with coefficients in [c1,c2,...] to a polynomial
--with coefficients in [d1,d2,...].
--Somewhat dodgy, as it depends on exact matching of coefficients...
convertCoeffs :: (Coefficient a, Coefficient b) => [a] -> [b] -> Polynomial a -> Polynomial b 
convertCoeffs _ _ [] = []
convertCoeffs cfs dfs (c:cs) = case lookup c cfs' of
                                    Nothing -> error "couldn't look up coeff; possible rounding error"
                                    Just cf' -> cf':(convertCoeffs cfs dfs cs)
    where cfs' = zipWith (\x y -> (y,x)) dfs cfs

--Note: colouring depends on the order of the coefficients.
--This is a "base d" expansion.
toGValue1 :: (Coefficient a) => IterCoeffs a -> Polynomial a -> Double
toGValue1 cfs p = z * evaluate p' z
    where d = fromIntegral (length cfs) -1
          p' = convertCoeffs cfs [0..d] p
          z =  1 / fromIntegral (length cfs)

--This one should be used with gradients such that g(0)=g(1).
--This uses the scale factors.
toGValue2 :: (Coefficient a) => IterCoeffs a -> Center -> Polynomial a -> Double
toGValue2 cfs c p = 0.5 + 1/(2*pi) * phase scale
    where scale = (negate . recip . (`evaluate` c)) $ derivative $ (map toComplex) $ p

