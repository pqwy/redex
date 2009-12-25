module Ast
    ( Term, VarID, SpliceID
    , AST(..), ast, foldTerm
    , var, app, lam, leet
    , freeVars, freeIn
    ) where

import Prelude hiding ( elem )

import SimpleSet


type VarID = String
type Vars = Set VarID
type SpliceID = String


data Term = Var_ VarID
          | App_ Vars Term Term
          | Lam_ Vars VarID Term
          | Let_ Vars VarID Term Term
          | Splice_ Vars Vars SpliceID
    -- deriving (Show, Eq)
    deriving (Eq)


foldTerm :: (VarID -> a)
         -> (a -> a -> a)
         -> (VarID -> a -> a)
         -> (VarID -> a -> a -> a)
         -> Term
         -> a
foldTerm var app lam leet = f
    where f (Var_ x)       = var x
          f (App_ _ l r)   = app (f l) (f r)
          f (Lam_ _ x m)   = lam x (f m)
          f (Let_ _ x e m) = leet x (f e) (f m)


freeVars :: Term -> Vars
freeVars (Var_ x) = singleton x
freeVars (App_ xs _ _) = xs
freeVars (Lam_ xs _ _) = xs
freeVars (Let_ xs _ _ _) = xs
freeVars (Splice_ xs _ _) = xs


freeIn :: VarID -> Term -> Bool
freeIn x t = x `elem` freeVars t


var :: VarID -> Term
var = Var_

app :: Term -> Term -> Term
app a b = App_ (freeVars a `union` freeVars b) a b

lam :: VarID -> Term -> Term
lam x m = Lam_ (x `remove` freeVars m) x m

leet :: VarID -> Term -> Term -> Term
leet x e m | x `freeIn` m = Let_ nufree x e m
           | otherwise = m
    where nufree = x `remove` (freeVars e `union` freeVars m)


data AST = Var VarID
         | App Term Term
         | Lam VarID Term
         | Let VarID Term Term
    -- deriving (Show, Eq)
    deriving (Eq)


ast :: Term -> AST
ast (Var_ x) = Var x
ast (App_ _ l r) = App l r
ast (Lam_ _ x m) = Lam x m
ast (Let_ _ x e m) = Let x e m



