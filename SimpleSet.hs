{-# LANGUAGE BangPatterns #-}
module SimpleSet
    ( Set
    , null, empty, singleton, fromList, size
    , insert, queryRemove, elem, remove
    , union, unions, intersection, subset
    ) where

import Prelude hiding ( elem, null )
import Data.List ( sort )
import Data.Maybe


data Set a = !a :+ !(Set a) | Nil
    deriving (Eq)

instance (Show a) => Show (Set a) where
    showsPrec _ Nil = ("empty" ++)
    showsPrec _ set = showString "fromList [" . unroll set . showString "]"
      where
        unroll (a :+ Nil) = shows a
        unroll (a :+ set) = shows a . showChar ',' . unroll set

null :: Set a -> Bool
null Nil = True
null _   = False

empty :: Set a
empty = Nil

singleton :: a -> Set a
singleton a = a :+ Nil

fromList :: (Ord a) => [a] -> Set a
fromList = nub' . sort
  where
    nub' []  = Nil
    nub' [a] = a :+ Nil
    nub' (a:as@(b:_)) | a == b    = nub' as
                      | otherwise = a :+ nub' as

size :: Set a -> Int
size = go 0 where go !n Nil     = n
                  go !n (_:+as) = go (succ n) as

insert :: (Ord a) => a -> Set a -> Set a
insert a Nil = singleton a
insert a xs@(x:+xs')
    | x <  a    = x :+ insert a xs'
    | x == a    = xs
    | otherwise = a :+ xs

queryRemove :: (Ord a) => a -> Set a -> Maybe (Set a)
queryRemove a Nil = Nothing
queryRemove a xs@(x:+xs')
    | x  < a    = (x:+) `fmap` queryRemove a xs'
    | x == a    = Just xs'
    | otherwise = Nothing

elem :: (Ord a) => a -> Set a -> Bool
elem a = isJust . queryRemove a

remove :: (Ord a) => a -> Set a -> Set a
remove a s = fromMaybe s (queryRemove a s)

union :: (Ord a) => Set a -> Set a -> Set a
union Nil bs  = bs
union as  Nil = as
union as@(a:+as') bs@(b:+bs') =
    case a `compare` b of
         LT -> a :+ union as' bs
         EQ -> a :+ union as' bs'
         GT -> b :+ union as  bs'

unions :: (Ord a) => [Set a] -> Set a
unions = foldr union empty

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection as@(a:+as') bs@(b:+bs')
    | a <  b    =      intersection as' bs
    | a == b    = a :+ intersection as' bs'
    | otherwise =      intersection as bs'
intersection _ _ = Nil

subset :: (Ord a) => Set a -> Set a -> Bool
subset Nil _ = True
subset as@(a:+as') bs@(b:+bs')
    | a  < b    = False
    | a == b    = subset as' bs'
    | otherwise = subset as bs'

