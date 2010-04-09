{-# LANGUAGE ViewPatterns, PatternGuards  #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Operations
    ( substitute, leet
    , alpha, lazyBeta, splicingBeta, pushLet
    ) where

import Ast

import Data.Function ( fix )
import Data.Maybe

import Data.Char ( isDigit )

-- tree xforms {{{

scrubFor :: Term -> Term -> AST

scrubFor t2 t@(ast -> Lam x m)
    | x `freeIn` t2 = let x' = x `newNameIn` [m, t2] in Lam x' (alpha x x' m)

scrubFor t2 t@(ast -> Let x e m)
    | x `freeIn` t2 = let x' = x `newNameIn` [e, m, t2]
                      in Let x' (alpha x x' e) (alpha x x' m)

scrubFor t2 (ast -> t) = t


substitute :: Term -> Ident -> Term -> Term
substitute s x t | x `notFreeIn` t = t

substitute s x (scrubFor s -> App a b) =
    app (substitute s x a) (substitute s x b)
substitute s x (scrubFor s -> Lam y m) =
    lam y (substitute s x m)
substitute s x (scrubFor s -> Let y e m) =
    fixLet y (substitute s x e) (substitute s x m)
substitute s x (scrubFor s -> Var _) = s
substitute s x (scrubFor s -> Mark t m) =
    mark t (substitute s x m)


leet, pushLet :: Ident -> Term -> Term -> Term

leet x e@(ast -> Var y) t | x /= y = substitute e x t
leet x e t                         = pushLet x e t


pushLet x e t | x `notFreeIn` t = t
pushLet x e t = case scrubFor e t of

        App a b              -> bifurcate app a b
        Let y e1 m           -> bifurcate (fixLet y) e1 m
        Lam y m              -> lam y (pushLet x e m)
        Mark t m             -> mark t (pushLet x e m)
        Var _ | x `freeIn` e -> fixLet x e t
              | otherwise    -> e

    where bifurcate f a b | x `notFreeIn` a = f a (pushLet x e b)
                          | x `notFreeIn` b = f (pushLet x e a) b
                          | otherwise = fixLet x e t


-- }}}

-- abvgd {{{

-- XXX want smarter renaming-renaming-renaming
-- newName :: Ident -> Vars -> Ident
-- newName (v:x:[]) vs | isDigit x = newName [v] vs
-- newName v vs = head [ y' | n <- [ 0 .. ]
--                          , let y' = v ++ show n , not (y' ^? vs) ]

newName :: Ident -> Vars -> Ident
newName (ID x) vs = newName (IDD x 0) vs
newName (IDD x n) vs | id ^? vs  = newName id vs
                     | otherwise = id
    where id = IDD x (n + 1)


newNameIn :: Ident -> [Term] -> Ident
newNameIn v = newName v . unions . map vars


alpha :: Ident -> Ident -> Term -> Term
alpha x y t | x `notFreeIn` t = t
alpha x y (ast -> App a b    ) = app (alpha x y a) (alpha x y b)
alpha x y (ast -> Lam x' m   ) = lam x' (alpha x y m)
alpha x y (ast -> Let x' e m ) = fixLet x' (alpha x y e) (alpha x y m)
alpha x y (ast -> Var _      ) = var y
alpha x y (ast -> Mark t m   ) = mark t (alpha x y m)


splicingBeta, lazyBeta :: Term -> Term -> Term

splicingBeta (ast -> Lam x m) t = substitute t x m
splicingBeta _ _ = error "splicingBeta: lambda?"

lazyBeta = flip lazyBeta'

lazyBeta' t (scrubFor t -> Lam x m) = leet x t m
lazyBeta' _ _ = error "lazyBeta: lambda?"

-- }}}


-- vim:set fdm=marker:
