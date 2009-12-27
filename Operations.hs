{-# LANGUAGE ViewPatterns, PatternGuards  #-}
{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Operations
    ( substitute, {- rotateLet,-} leet
    , alpha, lazyBeta, splicingBeta
    ) where

import Ast

import Data.Function ( fix )
import Data.Maybe
import qualified SimpleSet as S


-- tree xforms {{{

openScopeFor :: Term -> Term -> Term

openScopeFor t2 t@(ast -> Lam x m)
    | x `notFreeIn` t2 = t
    | otherwise = let x' = x `newNameIn` m in lam x' (alpha x x' m)

openScopeFor t2 t@(ast -> Let x e m)
    | x `notFreeIn` t2 = t
    | otherwise = let x' = newName x (vars e `S.union` vars m)
                  in fixLet x' (alpha x x' e) (alpha x x' m)

openScopeFor t2 t = t


substitute :: Term -> VarID -> Term -> Term
substitute s x t | x `notFreeIn` t = t

substitute s x (ast . openScopeFor s -> App a b) =
    app (substitute s x a) (substitute s x b)
substitute s x (ast . openScopeFor s -> Lam y m) =
    lam y (substitute s x m)
substitute s x (ast . openScopeFor s -> Let y e m) =
    fixLet y (substitute s x e) (substitute s x m)
substitute s x (ast . openScopeFor s -> Var _) = s



leet :: VarID -> Term -> Term -> Term
leet x e m | x `notFreeIn` m = m

leet x e t = case ast (openScopeFor e t) of

        App a b              -> bifurcate app a b
        Let y e1 m           -> bifurcate (fixLet y) e1 m
        Lam y m              -> lam y (leet x e m)
        Var _ | x `freeIn` e -> fixLet x e t
              | otherwise    -> e

    where bifurcate f a b | not (x `freeIn` a) = f a (leet x e b)
                          | not (x `freeIn` b) = f (leet x e a) b
                          | otherwise = fixLet x e t

-- }}}

-- abvgd {{{

newName :: VarID -> Vars -> VarID
newName v vs = head [ y' | n <- [ 0 .. ]
                         , let y' = v ++ show n
                         , not (y' `S.elem` vs) ]


newNameIn :: VarID -> Term -> VarID
newNameIn v = newName v . vars


alpha :: VarID -> VarID -> Term -> Term
alpha x y t | x `notFreeIn` t = t
            | otherwise =
                case ast t of
                     App a b    -> app (alpha x y a) (alpha x y b)
                     Lam x' m   -> lam x' (alpha x y m)
                     Let x' e m -> fixLet x' (alpha x y e) (alpha x y m)
                     Var _      -> var y


splicingBeta, lazyBeta :: Term -> Term -> Term

splicingBeta (ast -> Lam x m) t = substitute t x m
splicingBeta _ _ = error "splicingBeta: lambda?"

lazyBeta (ast -> Lam x m) t
        | x `notFreeIn` t = leet x t m
        | otherwise = let x' = x `newNameIn` m
                      in leet x' t (alpha x x' m)
lazyBeta _ _ = error "lazyBeta: lambda?"

-- }}}


-- vim:set fdm=marker:
