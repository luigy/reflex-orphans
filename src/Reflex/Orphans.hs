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
                        These a b -> return . Just $ a b
                        _ -> return Nothing) $ align (updated f) (updated x))
