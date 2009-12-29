{-# LANGUAGE ViewPatterns, PatternGuards  #-}
{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Reductions
    ( Reduceron, reduce, noenv
    , whnf, whnftw
    , whnf1, whnf2
    ) where


import Ast
import Operations
import Primitives

import Control.Monad
import "transformers" Control.Monad.Identity
import "transformers" Control.Monad.Trans
import "transformers" Control.Monad.Trans.State ( StateT(..) )
import "transformers" Control.Monad.Trans.List
import "monads-fd" Control.Monad.State 

import Control.Applicative


-- normalizations {{{


whnf beta t@(ast -> App f a) =
    whnf beta f >>= \f' ->
        case ast f' of
             Lam _ _ -> pure (app f' a) <|> whnf beta (beta f' a)
             _ -> pure (app f' a)

whnf beta t@(ast -> Let x e m) =
    flip (leet x) <$> (pushM x e *> whnf beta m) <*> popM

whnf beta t@(ast -> Lam _ _) = pure t

whnf beta t@(ast -> Var x) =
    resolveM x >>= ( maybe (pure t) $ \e' ->
        whnf beta e' >>= \t' -> assertM x t' *> pure t' )

whnf beta t@(ast -> Prim _) = pure t



--
-- whnftw b e t = desu b e t id (const (:[]))
-- 
-- 
-- desu :: (Term -> Term -> Term) -> Env -> Term -> (Term -> a) -> (Env -> Term -> [a]) -> [a]
-- 
-- desu bt en t@(ast -> Lam _ _) k1 k2 = k2 en t
-- 
-- desu bt en t@(ast -> App f a) k1 k2 =
--     desu bt en f (k1 . flip app a) $ \en' f' ->
--         case ast f' of
--              Lam _ _ -> k1 (app f' a) : desu bt en' (bt f' a) k1 k2
--              _       -> k2 en' (app f' a)
-- 
-- desu bt en t@(ast -> Let x e m) k1 k2 =
--     desu bt (push x e en) m (k1 . leet x e) $
--         \((_, e') : en') m' -> k2 en' (leet x e' m')
-- 
-- desu bt en t@(ast -> Var x@(resolve en -> Just e)) k1 k2 =
--     k1 t : ( desu bt en e k1 $ \en' e' ->
--                     k2 (assert x e' en') e' )
-- 
-- desu bt en t@(ast -> Var _) k1 k2 = k2 en t



whnftw b t = odesu b t pure pure


odesu :: (Term -> Term -> Term) -> Term -> (Term -> Reduceron a) -> (Term -> Reduceron a) -> Reduceron a

odesu bt t@(ast -> Lam _ _) k1 k2 = k2 t

odesu bt t@(ast -> App f a) k1 k2 =
    odesu bt f (k1 . flip app a) $ \f' ->
        case ast f' of
             Lam _ _ -> k1 (app f' a) <|> odesu bt (bt f' a) k1 k2
             _       -> k2 (app f' a)

odesu bt t@(ast -> Let x e m) k1 k2 =
    pushM x e *> odesu bt m (k1 . leet x e)
        ( \m' -> popM >>= \e' -> k2 (leet x e' m') )

odesu bt t@(ast -> Var x) k1 k2 =
    resolveM x >>= maybe (k2 t)
        ( \e' -> odesu bt e' k1 $ \e'' -> assertM x e'' *> k2 e'' )


-- }}}

type Env = [(VarID, Term)]

noenv = [] :: Env

push :: VarID -> Term -> Env -> Env
push x e en = (x, e) : en

pop :: Env -> Maybe ((VarID, Term), Env)
pop [] = Nothing
pop (xe:en) = Just (xe, en)

assert :: VarID -> Term -> Env -> Env
assert x e [] = error "no."
assert x e (ye@(y, _) : en)
    | x == y = (x, e) : en
    | otherwise = ye : assert x e en

resolve :: Env -> VarID -> Maybe Term
resolve = flip lookup


-- THE REDUCERON {{{

newtype Reduceron a = Red { reduce_ :: StateT Env (ListT Identity) a }
    deriving (Functor, Monad, MonadState Env, Applicative, Alternative)


-- reduce :: Reduceron a -> Env -> [a]
-- reduce r e = runIdentity $ runListT (reduce_ r `evalStateT` e)

reduce :: Reduceron a -> [a]
reduce r = runIdentity $ runListT (reduce_ r `evalStateT` noenv)


resolveM :: VarID -> Reduceron (Maybe Term)
resolveM x = gets (lookup x)

pushM :: VarID -> Term -> Reduceron ()
pushM x t = modify ((x, t) :)

popM :: Reduceron Term
popM = gets (snd . head) <* modify tail

assertM :: VarID -> Term -> Reduceron ()
assertM x t = modify (assert x t)

-- }}}


-- whnf1, whnf2 :: Term -> [Term]
-- whnf1 t = whnf splicingBeta t `reduce` noenv
-- whnf2 t = whnf lazyBeta t `reduce` noenv

whnf1, whnf2 :: Term -> [Term]
whnf1 t = reduce (whnf splicingBeta t)
whnf2 t = reduce (whnf lazyBeta t)

-- vim:set fdm=marker:
