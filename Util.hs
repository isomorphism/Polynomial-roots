module Util ( module Control.Applicative 
            , module Control.Monad
            , module Data.Maybe
            , module Util
            ) where

import Control.Monad
import Control.Applicative
import Data.Maybe

takeUntilDiff :: (a -> a -> Bool) -> [a] -> [a]
takeUntilDiff f (x1:x2:xs) | f x1 x2   = [x1]
                           | otherwise = x1:takeUntilDiff f (x2:xs)
takeUntilDiff _ xs = xs


justIf :: (a -> Bool) -> a -> Maybe a
justIf p x | p x 	   = Just x
           | otherwise = Nothing


(??) :: Maybe a -> a -> a
(??) = flip fromMaybe


unfoldActionM :: (Functor m, Monad m) => m (Maybe a) -> m [a]
unfoldActionM mx = step =<< mx
  where step (Just x) = (x:) <$> unfoldActionM mx
        step Nothing  = return []