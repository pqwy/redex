module Ast
    ( Term, VarID, SpliceID
    , AST(..), view, foldTerm
    , var, app, lam, leet
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
    deriving (Show, Eq)


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
freeVars (Splice_ xs _ _) = xs


var :: VarID -> Term
var = Var_

app :: Term -> Term -> Term
app a b = App_ (freeVars a `union` freeVars b) a b

lam :: VarID -> Term -> Term
lam x m = Lam_ (x `remove` freeVars m) x m

leet :: VarID -> Term -> Term -> Term
leet x e m | x `elem` freeVars m = Let_ nufree x e m
           | otherwise = m
    where nufree = x `remove` (freeVars e `union` freeVars m)


data AST = Var VarID
         | App Term Term
         | Lam VarID Term
         | Let VarID Term Term
    deriving (Show, Eq)


view :: Term -> AST
view (Var_ x) = Var x
view (App_ _ l r) = App l r
view (Lam_ _ x m) = Lam x m
view (Let_ _ x e m) = Let x e m


substitute :: Term -> VarID -> Term -> Term
substitute s x t
    | not (x `elem` freeVars t) = t
    | otherwise = case t of
        Var_ _       -> t
        Lam_ _ x m   -> lam x (substitute s x m)
        App_ _ a b   -> app (substitute s x a)
                            (substitute s x b)
        Let_ _ x e m -> leet x (substitute s x e)
                               (substitute s x m)
         -- Splice _ 



