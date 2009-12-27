module Ast
    ( Term, VarID, Vars, SpliceID
    , AST(..), ast
    , var, app, lam, fixLet
    , freeVars, freeIn, notFreeIn, vars, varIn
    ) where


import Prelude hiding ( elem )

import SimpleSet


type VarID = String
type Vars = Set VarID
type SpliceID = String


data Term = Var_ VarID
          | App_ Vars Vars Term Term
          | Lam_ Vars Vars VarID Term
          | Let_ Vars Vars VarID Term Term
          -- | Splice_ Vars Vars Vars SpliceID
    -- deriving (Show, Eq)
    deriving (Eq)


freeVars :: Term -> Vars
freeVars (Var_ x) = singleton x
freeVars (App_ xs _ _ _) = xs
freeVars (Lam_ xs _ _ _) = xs
freeVars (Let_ xs _ _ _ _) = xs
-- freeVars (Splice_ xs _ _) = xs

vars :: Term -> Vars
vars (Var_ x) = singleton x
vars (App_ _ xs _ _) = xs
vars (Lam_ _ xs _ _) = xs
vars (Let_ _ xs _ _ _) = xs


freeIn, notFreeIn :: VarID -> Term -> Bool
freeIn x t = x `elem` freeVars t
notFreeIn x t = not (freeIn x t)

varIn :: VarID -> Term -> Bool
varIn x t = x `elem` vars t


var :: VarID -> Term
var = Var_

app :: Term -> Term -> Term
app a b = App_ (freeVars a `union` freeVars b)
               (vars a `union` vars b)
               a b

lam :: VarID -> Term -> Term
lam x m = Lam_ (x `remove` freeVars m)
               (x `insert` vars m)
               x m


fixLet :: VarID -> Term -> Term -> Term
fixLet x e m = Let_ nufree nuvars x e m
    where nufree = x `remove` (freeVars e `union` freeVars m)
          nuvars = x `insert` (vars e `union` vars m)




data AST = Var VarID
         | App Term Term
         | Lam VarID Term
         | Let VarID Term Term
    -- deriving (Show, Eq)
    deriving (Eq)


ast :: Term -> AST
ast (Var_ x) = Var x
ast (App_ _ _ l r) = App l r
ast (Lam_ _ _ x m) = Lam x m
ast (Let_ _ _ x e m) = Let x e m



