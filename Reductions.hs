{-# LANGUAGE ViewPatterns, PatternGuards  #-}
{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Reductions
    ( Reduceron, reduce, noenv
    , whnf 
    , whnf1, whnf2
    ) where


import Ast
import Operations

import Control.Monad
import "transformers" Control.Monad.Identity
import "transformers" Control.Monad.Trans
import "transformers" Control.Monad.Trans.State ( StateT(..) )
import "transformers" Control.Monad.Trans.List
import "monads-fd" Control.Monad.State 

import Control.Applicative


-- normalizations {{{

whnf :: (Term -> Term -> Term) -> Term -> Reduceron Term

whnf beta t@(ast -> App f a) =
    whnf beta f >>= \f' ->
        case ast f' of
             Lam _ _ -> pure (app f' a) <|> whnf beta (beta f' a)
             -- Let _ _ _ | Just f'' <- rotateLet f' ->
             --                pure (app f' a) <|> pure (app f'' a)
             --            <|> whnf beta (beta f'' a)
             _ -> pure (app f' a)

whnf beta t@(ast -> Let x e m) =
    flip (leet x) <$> (push x e *> whnf beta m) <*> pop

whnf beta t@(ast -> Lam _ _) = pure t

whnf beta t@(ast -> Var x) =
    pure t <|> (resolve x >>= whnf beta >>= \t' -> assert x t' *> pure t')
    -- (resolve x >>= whnf beta >>= \t' -> assert x t' *> pure t')

-- }}}

-- THE REDUCERON {{{

type Env = [(VarID, Term)]

newtype Reduceron a = Red { reduce_ :: StateT Env (ListT Identity) a }
    deriving (Functor, Monad, MonadState Env, Applicative, Alternative)


-- reduce :: Reduceron a -> Env -> [a]
-- reduce r e = runIdentity $ runListT (reduce_ r `evalStateT` e)
reduce :: Reduceron a -> [a]
reduce r = runIdentity $ runListT (reduce_ r `evalStateT` noenv)

noenv = [] :: Env


resolve :: VarID -> Reduceron Term
resolve x = gets (lookup x) >>= maybe empty return

push :: VarID -> Term -> Reduceron ()
push x t = modify ((x, t) :)

pop :: Reduceron Term
pop = gets (snd . head) <* modify tail

assert :: VarID -> Term -> Reduceron ()
assert x t = modify (mod' x t)

    where mod' _ _ [] = error ("assert: var " ++ x ++" went missing?")
          mod' x t (yt@(y, t') : xts) | x == y = (y, t) : xts
                                      | otherwise = yt : mod' x t xts
-- }}}


-- whnf1, whnf2 :: Term -> [Term]
-- whnf1 t = whnf splicingBeta t `reduce` noenv
-- whnf2 t = whnf lazyBeta t `reduce` noenv

whnf1, whnf2 :: Term -> [Term]
whnf1 t = reduce (whnf splicingBeta t)
whnf2 t = reduce (whnf lazyBeta t)

-- vim:set fdm=marker:
