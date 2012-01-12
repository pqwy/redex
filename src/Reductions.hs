{-# LANGUAGE ViewPatterns, PatternGuards  #-}
{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses  #-}

module Reductions
    ( Reduceron, reduce, noenv
    , whnf, whnftw
    , whnf1, whnf2, whnf3, whnf4, whnf5, hnf1, hnf2
    ) where


import Ast
import Operations
--  import Primitives

import Control.Monad
import "transformers" Data.Functor.Identity
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

--  whnf beta t@(ast -> Prim _) = pure t




-- whnftw' b e t = desu b e t id (const (:[]))


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
--     desu bt en e k1 $ \en' e' ->
--                 k2 (assert x e' en') e'
-- 
-- desu bt en t@(ast -> Var _) k1 k2 = k2 en t



whnftw' b e t = desu b (\_ e t _ k -> k e t) e t (const id) (const (:[]))

hnftw' b e t = desu b drop e t (const id) (const (:[]))
    where
        drop c e (ast -> Lam x m) k1 k2 = c e m (\e' m' -> k1 e' (lam x m'))
                                                (\e' m' -> k2 e' (lam x m'))


-- desu :: (Term -> Term -> Term) -> Env -> Term -> (Env -> Term -> a) -> (Env -> Term -> [a]) -> [a]

-- desu bt en t@(ast -> Lam _ _) k1 k2 = k2 en t
desu bt lm en t@(ast -> Lam _ _) k1 k2 = lm (desu bt lm) en t k1 k2

-- desu bt en t@(ast -> App f a) k1 k2 =
--     desu bt en f (\en' f' -> k1 en' (app f' a)) $ \en' f' ->
--         case ast f' of
--              -- Lam _ _ -> k1 en' (app f' a) : desu bt en' (bt f' a) k1 k2
--              Lam _ _ -> k1 en' (markS "app" $ app f' a)
--                             : desu bt en' (bt f' a) k1 k2
--              _       -> k2 en' (app f' a)
desu bt lm en t@(ast -> App f a) k1 k2 =
    desu bt lm en f (\en' f' -> k1 en' (app f' a)) $ \en' f' ->
        case ast f' of
             -- Lam _ _ -> k1 en' (app f' a) : desu bt en' (bt f' a) k1 k2
             Lam _ _ -> k1 en' (markS "app" $ app f' a)
                            : desu bt lm en' (bt f' a) k1 k2
             _       -> k2 en' (app f' a)

-- desu bt en t@(ast -> Let x e m) k1 k2 =
--     let reco k ((_, e'):en') m' = k en' (leet x e' m') in
--     desu bt (push x e en) m (reco k1) (reco k2)
desu bt lm en t@(ast -> Let x e m) k1 k2 =
    let reco k ((_, e'):en') m' = k en' (leet x e' m') in
    desu bt lm (push x e en) m (reco k1) (reco k2)

-- desu bt en t@(ast -> Var x@(resolve en -> Just e)) k1 k2 =
--     -- desu bt en e (\en' e' -> k1 (assert x e' en') t)
--     --              (\en' e' -> k2 (assert x e' en') e')
--     k1 en (markS ("res " ++ x) t) :
--     desu bt en e (\en' e' -> k1 (assert x e' en') t)
--                  (\en' e' -> k2 (assert x e' en') e')
desu bt lm en t@(ast -> Var x@(resolve en -> Just e)) k1 k2 =
    -- desu bt en e (\en' e' -> k1 (assert x e' en') t)
    --              (\en' e' -> k2 (assert x e' en') e')
    k1 en (markS ("res " ++ show x) t) :
    desu bt lm en e (\en' e' -> k1 (assert x e' en') t)
                    (\en' e' -> k2 (assert x e' en') e')

desu bt lm en t@(ast -> Var _) k1 k2 = k2 en t



whnftw b t = odesu b t pure pure


odesu :: (Term -> Term -> Term) -> Term -> (Term -> Reduceron a) -> (Term -> Reduceron a) -> Reduceron a

odesu bt t@(ast -> Lam _ _) k1 k2 = k2 t

odesu bt t@(ast -> App f a) k1 k2 =
    odesu bt f (k1 . flip app a) $ \f' ->
        case ast f' of
             Lam _ _ -> k1 (app f' a) <|> odesu bt (bt f' a) k1 k2
             _       -> k2 (app f' a)

odesu bt t@(ast -> Let x e m) k1 k2 =
    let reco k m' = popM >>= \e' -> k (leet x e' m') in
    pushM x e *> odesu bt m (reco k1) (reco k2)

odesu bt t@(ast -> Var x) k1 k2 =
    resolveM x >>= maybe (k2 t) ( \e' ->
            let reco k e'' = assertM x e'' *> k e'' in
            odesu bt e' (reco k1) (reco k2) )

-- odesu :: (Term -> Term -> Term) -> Term -> (Term -> Reduceron a) -> (Term -> Reduceron a) -> Reduceron a
-- 
-- odesu bt t@(ast -> Lam _ _) k1 k2 = k2 t
-- 
-- odesu bt t@(ast -> App f a) k1 k2 =
--     odesu bt f (k1 . flip app a) $ \f' ->
--         case ast f' of
--              Lam _ _ -> k1 (app f' a) <|> odesu bt (bt f' a) k1 k2
--              _       -> k2 (app f' a)
-- 
-- odesu bt t@(ast -> Let x e m) k1 k2 =
--     pushM x e *> odesu bt m (k1 . leet x e)
--         ( \m' -> popM >>= \e' -> k2 (leet x e' m') )
-- 
-- odesu bt t@(ast -> Var x) k1 k2 =
--     resolveM x >>= maybe (k2 t) ( \e' ->
--             odesu bt e' k1 $ \e'' -> assertM x e'' *> k2 e'' )
-- 

-- }}}


type Env = [(Ident, Term)]

noenv = [] :: Env

push :: Ident -> Term -> Env -> Env
push x e en = (x, e) : en

pop :: Env -> Maybe ((Ident, Term), Env)
pop [] = Nothing
pop (xe:en) = Just (xe, en)

assert :: Ident -> Term -> Env -> Env
assert x e [] = error "no."
assert x e (ye@(y, _) : en)
    | x == y = (x, e) : en
    | otherwise = ye : assert x e en

resolve :: Env -> Ident -> Maybe Term
resolve = flip lookup


-- THE REDUCERON {{{

newtype Reduceron a = Red { reduce_ :: Env -> [(a, Env)] }

instance Functor Reduceron where
    fmap f (Red g) = Red (\e -> [ (f a, e') | (a, e') <- g e ])

instance Monad Reduceron where
    return a = Red (\e -> [(a, e)])
    Red m >>= f = Red $ \e ->
        [ be | (a, e') <- m e, be <- reduce_ (f a) e' ]

instance MonadState Env Reduceron where
    get = Red (\s -> [(s, s)])
    put s = Red (\_ -> [((), s)])

instance Applicative Reduceron where
    pure = return
    (<*>) = ap

instance Alternative Reduceron where
    empty = Red (\_ -> [])
    Red a <|> Red b = Red (\s -> a s ++ b s)


-- newtype Reduceron a = Red { reduce_ :: StateT Env (ListT Identity) a }
--     deriving (Functor, Monad, MonadState Env, Applicative, Alternative)
-- 
-- 
-- -- reduce :: Reduceron a -> Env -> [a]
-- -- reduce r e = runIdentity $ runListT (reduce_ r `evalStateT` e)

reduce :: Reduceron a -> [a]
-- reduce r = runIdentity $ runListT (reduce_ r `evalStateT` noenv)
reduce r = map fst $ r `reduce_` noenv


resolveM :: Ident -> Reduceron (Maybe Term)
resolveM x = gets (lookup x)

pushM :: Ident -> Term -> Reduceron ()
pushM x t = modify ((x, t) :)

popM :: Reduceron Term
popM = gets (snd . head) <* modify tail

assertM :: Ident -> Term -> Reduceron ()
assertM x t = modify (assert x t)

-- }}}


-- whnf1, whnf2 :: Term -> [Term]
-- whnf1 t = whnf splicingBeta t `reduce` noenv
-- whnf2 t = whnf lazyBeta t `reduce` noenv

whnf1, whnf2 :: Term -> [Term]
whnf1 = seqlist . reduce . whnf splicingBeta
whnf2 = seqlist . reduce . whnf lazyBeta
whnf3 = seqlist . reduce . whnftw lazyBeta 
whnf4 = seqlist . whnftw' lazyBeta noenv
whnf5 = seqlist . whnftw' splicingBeta noenv
hnf1 = seqlist . hnftw' splicingBeta noenv
hnf2 = seqlist . hnftw' lazyBeta noenv


seqlist [] = []
seqlist (x:xs) = x `seq` (x : seqlist xs)


-- vim:set fdm=marker:
