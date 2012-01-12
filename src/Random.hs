module Random (
  ($>)
) where

infix 4 $>
($>) :: (Functor f) => f a -> (a -> b) -> f b
($>) = flip fmap
