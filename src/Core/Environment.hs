module Core.Environment (
    Environment, lookup, (-->), env, ids, elems
) where

import Prelude hiding ( lookup )

import Core.Ast

import Data.Monoid
import qualified Data.Map as M

newtype Environment t = E { e :: M.Map Ident t }
    deriving (Show)

lookup :: Ident -> Environment t -> Maybe t
lookup i (E e) = i `M.lookup` e

infix 8 -->
(-->) :: Ident -> t -> Environment t
(-->) = (E.) . M.singleton

env :: [(Ident, t)] -> Environment t
env = E . M.fromList

ids :: Environment t -> [Ident]
ids = M.keys . e

elems :: Environment t -> [t]
elems = M.elems . e

-- Maps are a monoid wrt left-biased union.
instance Monoid (Environment t) where
    mempty              = E M.empty
    E e1 `mappend` E e2 = E (e1 `M.union` e2)
    mconcat             = E . mconcat . map e

instance Functor Environment where
    fmap f = E . fmap f . e

