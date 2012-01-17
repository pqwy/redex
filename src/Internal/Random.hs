module Internal.Random (
  module Data.Monoid
  , ($>), (|+|)
) where

import Data.Monoid

infix 4 $>
($>) :: (Functor f) => f a -> (a -> b) -> f b
($>) = flip fmap

infixl 0 |>
(|>) = flip ($)

infixl 7 |+|
(|+|) :: Monoid a => a -> a -> a
(|+|) = mappend

