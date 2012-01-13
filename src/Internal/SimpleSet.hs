{-# LANGUAGE BangPatterns #-}

module Internal.SimpleSet
    ( Set
    , null, empty, singleton, fromList, toList, size
    , insert, elem, remove, (\\)
    , union, unions, intersection, subset
    ) where

import Prelude hiding ( elem, null )
import Data.List ( sort )
import Data.Maybe
import Data.Monoid ( Monoid (..) )


data Set a = !a :+ !(Set a) | Nil
    deriving (Eq)

instance (Show a) => Show (Set a) where
    showsPrec _ Nil = ("empty" ++)
    showsPrec _ set = showString "{" . unroll set . showString "}"
      where
        unroll (a :+ Nil) = shows a
        unroll (a :+ set) = shows a . showChar ',' . unroll set

instance Ord a => Monoid (Set a) where
    mappend = union
    mempty  = empty
    mconcat = unions

null :: Set a -> Bool
null Nil = True
null _   = False

empty :: Set a
empty = Nil

singleton :: a -> Set a
singleton = (:+ Nil)

fromList :: (Ord a) => [a] -> Set a
fromList = foldr nub Nil . sort
  where
    nub e r@(a:+_) | e == a = r
    nub e r                 = e :+ r

size :: Set a -> Int
size = go 0 where go !n Nil       = n
                  go !n (_ :+ as) = go (succ n) as

locating :: (Ord a) => ((Set a -> Set a) -> Set a -> Maybe a -> b) -> a -> Set a -> b
locating f x = go id
  where
    go k Nil = f k Nil Nothing
    go k s@(a :+ as) | a  < x    = go ((k $!).(a :+)) as
                     | a == x    = f k as (Just a)
                     | otherwise = f k s Nothing

insert :: (Ord a) => a -> Set a -> Set a
insert a s = locating (\k as -> maybe (k (a :+ as)) (\_ -> s)) a s

remove :: (Ord a) => a -> Set a -> Set a
remove = locating $ \k as _ -> k as

elem :: (Ord a) => a -> Set a -> Bool
elem = locating $ \_ _ -> maybe False (\_ -> True)

pfoldr :: (Ord a)
       => (a -> b -> b) -> (a -> b -> b) -> (a -> b -> b)
       -> b -> Set a -> Set a -> b

pfoldr f g h s = go
  where
    go    Nil         Nil      = s
    go    (a:+as)     Nil      = f a (go as Nil)
    go    Nil         (b:+bs)  = g b (go Nil bs)
    go as@(a:+as') bs@(b:+bs') =
        case a `compare` b of
             LT -> f a (go as' bs )
             GT -> g b (go as  bs')
             EQ -> h a (go as' bs')

union :: (Ord a) => Set a -> Set a -> Set a
union = pfoldr (:+) (:+) (:+) empty

unions :: (Ord a) => [Set a] -> Set a
unions = foldr union empty

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection = pfoldr (\_ -> id) (\_ -> id) (:+) empty

subset :: (Ord a) => Set a -> Set a -> Bool
subset = pfoldr (\_ _ -> False) (\_ b -> b) (\_ b -> b) True

(\\) :: (Ord a) => Set a -> Set a -> Set a
(\\) = pfoldr (:+) (\_ b -> b) (\_ b -> b) Nil

toList :: (Ord a) => Set a -> [a]
toList = pfoldr undefined (:) undefined [] Nil

