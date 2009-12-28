{-# LANGUAGE ViewPatterns, PatternGuards  #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Operations
    ( substitute, leet
    , alpha, lazyBeta, splicingBeta
    ) where

import Ast

import Data.Function ( fix )
import Data.Maybe


-- tree xforms {{{

openScopeFor :: Term -> Term -> Term

openScopeFor t2 t@(ast -> Lam x m)
    | x `notFreeIn` t2 = t
    | otherwise = let x' = x `newNameIn` [m, t2] in lam x' (alpha x x' m)

openScopeFor t2 t@(ast -> Let x e m)
    | x `notFreeIn` t2 = t
    | otherwise = let x' = x `newNameIn` [e, m, t2]
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



leet, spliceLet, pushLet :: VarID -> Term -> Term -> Term

leet x e@(ast -> Var _) t = spliceLet x e t
leet x e t                = pushLet x e t


spliceLet x e t | x `notFreeIn` t = t
spliceLet x e (ast -> App a b)    = app (spliceLet x e a) (spliceLet x e b)
spliceLet x e (ast -> Let y e1 m) = fixLet y (spliceLet x e e1) (spliceLet x e m)
spliceLet x e (ast -> Lam y m)    = lam y (spliceLet x e m)
spliceLet x e (ast -> Var _)      = e


pushLet x e t | x `notFreeIn` t = t
pushLet x e t = case ast (openScopeFor e t) of

        App a b              -> bifurcate app a b
        Let y e1 m           -> bifurcate (fixLet y) e1 m
        Lam y m              -> lam y (pushLet x e m)
        Var _ | x `freeIn` e -> fixLet x e t
              | otherwise    -> e

    where bifurcate f a b | x `notFreeIn` a = f a (pushLet x e b)
                          | x `notFreeIn` b = f (pushLet x e a) b
                          | otherwise = fixLet x e t

-- }}}

-- abvgd {{{

newName :: VarID -> Vars -> VarID
newName v vs = head [ y' | n <- [ 0 .. ]
                         , let y' = v ++ show n
                         , not (y' ^? vs) ]


newNameIn :: VarID -> [Term] -> VarID
newNameIn v = newName v . unions . map vars


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
        | otherwise = let x' = x `newNameIn` [m, t]
                      in leet x' t (alpha x x' m)
lazyBeta _ _ = error "lazyBeta: lambda?"

-- }}}


-- vim:set fdm=marker:
