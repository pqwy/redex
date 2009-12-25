{-# LANGUAGE ViewPatterns  #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Operations where

import Prelude hiding ( elem )

import Ast
import SimpleSet ( elem )

import Control.Applicative


substitute :: Term -> VarID -> Term -> Term
substitute s x t | not (x `elem` freeVars t) = t
substitute s x (ast -> Var _)     = s
substitute s x (ast -> Lam y m)   = lam y (substitute s x m)
substitute s x (ast -> App a b)   = app (substitute s x a)
                                        (substitute s x b)
substitute s x (ast -> Let y e m) = leet y (substitute s x e)
                                           (substitute s x m)


beta :: Term -> Term -> Term
beta (ast -> Lam x m) t = substitute t x m
beta _ _ = error "beta: term not lambda abstraction"

-- nf :: Term -> Term
-- nf t@(ast -> Lam x m) = lam x (nf m)
-- nf t@(

whnf' :: Term -> Maybe Term
whnf' (ast -> App f@(ast -> Lam _ _) a) = pure (beta f a)
whnf' (ast -> App f a)                  =
    do t' <- flip app a <$> whnf' f
       whnf' t' <|> pure t'
whnf' _                                 = Nothing


