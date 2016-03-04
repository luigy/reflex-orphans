module Reflex.Orphans () where

import Reflex

instance Reflex t => Functor (Dynamic t) where
    fmap f d = unsafeDynamic (fmap f . current $ d) (fmap f . updated $ d)
