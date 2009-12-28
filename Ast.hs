module Ast
    ( Term, VarID, Vars, SpliceID
    , AST(..), ast
    , var, app, lam, fixLet
    , freeVars, freeIn, notFreeIn, vars, varIn
    , (^+), (^?), (^<-), (^->), singleton, unions
    ) where


import Prelude hiding ( elem )

import qualified SimpleSet as S


type VarID = String
type SpliceID = String

type Vars = S.Set VarID


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
freeIn x t = x ^? freeVars t
notFreeIn x t = not (freeIn x t)

varIn :: VarID -> Term -> Bool
varIn x t = x ^? vars t


var :: VarID -> Term
var = Var_

app :: Term -> Term -> Term
app a b = App_ (freeVars a ^+ freeVars b)
               (vars a ^+ vars b)
               a b

lam :: VarID -> Term -> Term
lam x m = Lam_ (x ^<- freeVars m)
               (x ^-> vars m)
               x m


fixLet :: VarID -> Term -> Term -> Term
fixLet x e m = Let_ nufree nuvars x e m
    where nufree = x ^<- (freeVars e ^+ freeVars m)
          nuvars = x ^-> (vars e ^+ vars m)




data AST = Var VarID
         | App Term Term
         | Lam VarID Term
         | Let VarID Term Term


ast :: Term -> AST
ast (Var_ x) = Var x
ast (App_ _ _ l r) = App l r
ast (Lam_ _ _ x m) = Lam x m
ast (Let_ _ _ x e m) = Let x e m




singleton :: VarID -> Vars
singleton = S.singleton

unions :: [Vars] -> Vars
unions = S.unions

infixr 5 ^+
(^+) :: Vars -> Vars -> Vars
(^+) = S.union

infixr 7 ^<-, ^->
(^<-), (^->) :: VarID -> Vars -> Vars
(^<-) = S.remove
(^->) = S.insert

infix 4 ^?
(^?) :: VarID -> Vars -> Bool
(^?) = S.elem


