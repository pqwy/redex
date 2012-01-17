{-# LANGUAGE ViewPatterns, BangPatterns #-}
module Main where

import Internal.SimpleSet
import Data.Maybe
import Control.Arrow
import System.Random.Mersenne.Pure64
import System.Environment

foldNRandoms' :: (a -> Int -> a) -> a -> Int -> PureMT -> (a, PureMT)
foldNRandoms' f !s 0 mt = (s, mt)
foldNRandoms' f !s n mt =
    case randomInt mt of
         (!x, !mt') -> foldNRandoms' f (f s x) (pred n) mt'

randomSet :: Int -> PureMT -> (Set Int, PureMT)
randomSet = foldNRandoms' (flip insert) empty

qrandomSet :: Int -> PureMT -> (Set Int, PureMT)
qrandomSet = (first fromList.) . foldNRandoms' (flip (:)) []

--  randomQuery :: Set Int -> Int -> PureMT -> (Set Int, PureMT)
--  randomQuery = foldNRandoms' (\s x -> fromMaybe s (queryRemove'' x s))

randomRemove :: Set Int -> Int -> PureMT -> (Set Int, PureMT)
randomRemove = foldNRandoms' (flip remove)

main = do
    (read -> n) : _ <- getArgs
    mt              <- newPureMT
    let !(!s1, mt') = qrandomSet n mt
--      let !(!s1, mt' ) = randomSet n mt
--          !(!s2, mt'') = randomQuery s1 n mt'
--          !(!s2, mt'') = randomRemove s1 n mt'
    return ()
--      newPureMT >>= print . randomSet n


