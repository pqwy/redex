{-# LANGUAGE ViewPatterns, PatternGuards  #-}
{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Operations where

import Ast

import Data.Maybe

import Control.Monad
import "transformers" Control.Monad.Identity
import "transformers" Control.Monad.Trans
import "transformers" Control.Monad.Trans.State ( StateT(..) )
import "transformers" Control.Monad.Trans.List
import "monads-fd" Control.Monad.State 

import Control.Applicative



substitute :: Term -> VarID -> Term -> Term
substitute s x t | not (x `freeIn` t) = t

substitute s x (ast -> Var _)     = s
substitute s x (ast -> Lam y m)   =
    lam y (substitute s x m)

substitute s x (ast -> App a b)   =
    app (substitute s x a) (substitute s x b)

substitute s x (ast -> Let y e m) =
    leet y (substitute s x e) (substitute s x m)


rotateLet :: Term -> Maybe Term
rotateLet t@(ast -> Let _ _ _) =
    fix ( \f t k -> case ast t of
                         Let x e m -> f m (k . leet x e)
                         Lam x m   -> k m x
                         _         -> Nothing )
        t (\m x -> Just (lam x m))
rotateLet _ = Nothing


pushLet :: VarID -> Term -> Term -> Maybe Term

pushLet x e t | not (x `freeIn` t) = Nothing

pushLet x e (ast -> App a b)    = bifurcate x e app a b
pushLet x e (ast -> Let y e1 m) = bifurcate x e (leet y) e1 m
pushLet x e (ast -> Lam y m)    = lam y <$> pushLet x e m
pushLet x e (ast -> Var _)      = pure e


bifurcate x e f a b | pa && pb = pure (leet x e (f a b))
                    | pa = flip f b <$> (pushLet x e a)
                    | pb = f a <$> (pushLet x e b)

    where (pa, pb) = (x `freeIn` a, x `freeIn` b)



beta f (ast -> Lam x m) t = f x m t
beta _ _ _ = error "beta: term is not a lambda abstraction"


splicingBeta, lazyBeta :: Term -> Term -> Term

splicingBeta = beta (\x m t -> substitute t x m)
lazyBeta = beta (\x m t -> leet x t m)


-- nf :: Term -> Term
-- nf t@(ast -> Lam x m) = lam x (nf m)
-- nf t@(

-- whnf' :: Term -> Maybe Term
-- whnf' (ast -> App f@(ast -> Lam _ _) a) = pure (beta f a)
-- whnf' (ast -> App f a) =
--     do t' <- flip app a <$> whnf' f
--        whnf' t' <|> pure t'
-- whnf' _ = Nothing


-- w2 :: (Monad f, Alternative f) => Term -> f Term
-- w2 t@(ast -> App f@(ast -> Lam _ _) a) = pure t <|> w2 (beta f a)
-- w2 t@(ast -> App f a) =
--     do f' <- w2 f
--        let t' = app f' a
--        case ast f' of
--             Lam _ _ -> w2 t'
--             _       -> pure t'
-- w2 t = pure t


-- etaClean :: Term -> Term
-- etaClean t = maybe t id (ec t)
-- 
--     where ec t@(ast -> Lam x (ast -> App f (ast -> Var y)))
--                 | x == y = (ec f >>= ec) <|> pure f
--           ec (ast -> Lam x m) = lam x <$> (ec m) >>= ec
--           ec (ast -> App a b) = app <$> (ec a) <*> (ec b)
--           ec _ = empty


type Env = [(VarID, Term)]

newtype Reduceron a = Red { reduce_ :: StateT Env (ListT Identity) a }
    deriving (Functor, Monad, MonadState Env, Applicative, Alternative)


reduce :: Reduceron a -> Env -> [a]
reduce r e = runIdentity $ runListT (reduce_ r `evalStateT` e)

noenv = [] :: Env


resolve :: VarID -> Reduceron Term
resolve x = gets (lookup x) >>= maybe empty return

push :: VarID -> Term -> Reduceron ()
push x t = modify ((x, t) :)

pop :: Reduceron Term
pop = gets (snd . head) <* modify tail

assert :: VarID -> Term -> Reduceron ()
assert x t = modify (mod' x t)
    where mod' _ _ [] = error "assert: variable not active"
          mod' x t (yt@(y, t') : xts) | x == y = (y, t) : xts
                                      | otherwise = yt : mod' x t xts


-- w3 :: Term -> Reduceron Term
-- 
-- w3 t@(ast -> App f a) = 
--     w3 f >>= \f' -> case ast f' of
--                          Lam _ _ -> pure (app f' a) <|> w3 (splicingBeta f' a)
--                          Let _ _ _ | Just f'' <- rotateLet f' ->
--                                         pure (app f'' a) <|> w3 (splicingBeta f'' a)
--                          _       -> pure (app f' a)
-- 
-- -- w3 t@(ast -> Let x e m) = leetMe <$> (push x e *> w3 m) <*> pop
-- --     where leetMe m' e' = fromJust (pushLet x e' m' <|> pure m')
-- 
-- w3 t@(ast -> Let x e m) =
--     flip (leet x) <$> (push x e *> w3 m) <*> pop
-- 
-- w3 t@(ast -> Lam _ _) = pure t
-- 
-- w3 t@(ast -> Var x) =
--     pure t <|> (resolve x >>= w3 >>= \t' -> assert x t' *> pure t')
-- 
-- w3 t = pure t
-- 
-- 
-- 
-- w4 :: Term -> Reduceron Term
-- 
-- w4 t@(ast -> App f a) =
--     w4 f >>= \f' -> case ast f' of
--                          Lam _ _   -> pure (app f' a) <|> w4 (lazyBeta f' a)
--                          Let _ _ _ | Just f'' <- rotateLet f' ->
--                                         pure (app f' a) <|> pure (app f'' a)
--                                                         <|> w4 (lazyBeta f'' a)
--                          _         -> pure (app f' a)
-- 
-- w4 t@(ast -> Let x e m) =
--     flip (leet x) <$> (push x e *> w4 m) <*> pop
-- 
-- w4 t@(ast -> Lam _ _) = pure t
-- 
-- w4 t@(ast -> Var x) =
--     pure t <|> (resolve x >>= w4 >>= \t' -> assert x t' *> pure t')
-- 
-- w4 t = pure t
-- 


whnf :: (Term -> Term -> Term) -> Term -> Reduceron Term

whnf beta t@(ast -> App f a) =
    whnf beta f >>= \f' ->
        case ast f' of
             Lam _ _ -> pure (app f' a) <|> whnf beta (beta f' a)
             Let _ _ _ | Just f'' <- rotateLet f' ->
                            pure (app f' a) <|> pure (app f'' a)
                        <|> whnf beta (beta f'' a)
             _ -> pure (app f' a)

whnf beta t@(ast -> Let x e m) =
    flip (leet x) <$> (push x e *> whnf beta m) <*> pop

whnf beta t@(ast -> Lam _ _) = pure t

whnf beta t@(ast -> Var x) =
    pure t <|> (resolve x >>= whnf beta >>= \t' -> assert x t' *> pure t')


