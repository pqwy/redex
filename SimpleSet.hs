{-# LANGUAGE ViewPatterns, PatternGuards  #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module SimpleSet
    ( Set
    , null, empty, singleton, fromList
    , insert, queryRemove, elem, remove
    , union, unions, intersection, subset
    ) where


import Prelude hiding ( elem, null )
import Data.List ( sort )


cmp :: (Ord a) => a -> a -> Ordering
cmp = flip compare



newtype Set a = Set [a]
    deriving (Eq)


instance (Show a) => Show (Set a) where
    show (Set []) = "empty"
    show (Set [a]) = "singleton " ++ show a
    show (Set as) = "fromList " ++ show as


null :: Set a -> Bool
null (Set []) = True
null _ = False

empty :: Set a
empty = Set []

singleton :: a -> Set a
singleton a = Set [a]

fromList :: (Ord a) => [a] -> Set a
fromList = Set . nub' . sort

    where nub' [] = []
          nub' as@[a] = as
          nub' (a : as@(b:_)) | a == b = nub' as
                              | otherwise = a : nub' as


insert :: (Ord a) => a -> Set a -> Set a
insert a (Set s) = Set (doit s)

    -- where doit [] = []
    --       doit (x@(cmp a -> LT) : xs) = x : doit xs
    --       doit s@((cmp a -> EQ) : _)  = s
    --       doit s@((cmp a -> GT) : _)  = a : s

    where doit [] = [a]
          doit s@(x:xs) =
              case a `compare` x of
                   EQ -> s
                   LT -> a : s
                   GT -> x : doit xs


queryRemove :: (Ord a) => a -> Set a -> Maybe (Set a)
queryRemove a (Set xs) = Set `fmap` doit xs

    -- where doit [] = Nothing
    --       doit (x@(cmp a -> LT) : xs) = (x:) `fmap` doit xs
    --       doit ((cmp a -> EQ) : xs)   = Just xs
    --       doit ((cmp a -> GT) : _)    = Nothing

    where doit [] = Nothing
          doit (x:xs) =
              case a `compare` x of
                   EQ -> Just xs
                   LT -> Nothing
                   GT -> (x:) `fmap` doit xs


elem :: (Ord a) => a -> Set a -> Bool
elem a (queryRemove a -> Just _) = True
elem a _ = False


remove :: (Ord a) => a -> Set a -> Set a
remove a (queryRemove a -> Just s) = s
remove a s = s


union :: (Ord a) => Set a -> Set a -> Set a
union (Set a) (Set b) = Set (doit a b)

    -- where doit [] bs = bs
    --       doit as [] = as
    --       doit (a:as)   ((cmp a -> EQ) : bs)   = a : doit as bs
    --       doit as@(a:_) (b@(cmp a -> LT) : bs) = b : doit as bs
    --       doit (a:as)   bs@((cmp a -> GT) : _) = a : doit as bs

    where doit [] bs = bs
          doit as [] = as
          doit a'@(a:as) b'@(b:bs) =
              case a `compare` b of
                   EQ -> a : doit as bs
                   LT -> a : doit as b'
                   GT -> b : doit a' bs


unions :: (Ord a) => [Set a] -> Set a
unions = foldr union empty


intersection :: (Ord a) => Set a -> Set a -> Set a
intersection (Set a) (Set b) = Set (doit a b)

    -- where doit (a:as)   ((cmp a -> EQ) : bs)   = a : doit as bs
    --       doit (a:as)   bs@((cmp a -> GT) : _) = doit as bs
    --       doit as@(a:_) ((cmp a -> LT) : bs)   = doit as bs
    --       doit _ _ = []

    where doit a'@(a:as) b'@(b:bs) =
              case a `compare` b of
                   EQ -> a : doit as bs
                   LT -> doit as b'
                   GT -> doit a' bs
          doit _ _ = []


subset :: (Ord a) => Set a -> Set a -> Bool
subset (Set a) (Set b) = doit a b

    where doit [] _ = True
          doit a'@(a:as) (b:bs) =
              case a `compare` b of
                   EQ -> doit as bs
                   LT -> False
                   GT -> doit a' bs

    -- where doit [] _ = True
    --       doit as@(a:_) ((cmp a -> LT) : bs) = doit as bs
    --       doit (a:as)   ((cmp a -> EQ) : bs) = doit as bs
    --       doit (a:_)    ((cmp a -> GT) : _)  = False

