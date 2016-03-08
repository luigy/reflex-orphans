{-# LANGUAGE LambdaCase #-}
module Reflex.Orphans () where

import Data.These
import Data.Align
import Reflex

instance Reflex t => Functor (Dynamic t) where
    fmap f d = unsafeDynamic (fmap f . current $ d) (fmap f . updated $ d)

instance Reflex t => Applicative (Dynamic t) where
    pure = constDyn
    f <*> x = unsafeDynamic
              ((current f) <*> (current x))
              (push (\case
                        This a -> (Just . a) <$> (sample . current $ x)
                        That b -> (Just . ($ b)) <$> (sample . current $ f)
                        These a b -> return . Just $ a b) $ align (updated f) (updated x))
