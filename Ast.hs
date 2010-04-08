module Ast
    ( Term, VarID, Vars
    , AST(..), ast
    , var, app, lam, fixLet, prim, mark, mark', markS
    , freeVars, freeIn, notFreeIn, vars, varIn
    , (^+), (^?), (^<-), (^->), noVars, singleton, unions
    , PrimRep, Primitive(..), arity, primrep, combineRep, num
    ) where


import Prelude hiding ( elem )

import qualified SimpleSet as S


type VarID = String

type Vars = S.Set VarID


data Term = Var_ VarID
          | App_ Vars Vars Term Term
          | Lam_ Vars Vars VarID Term
          | Let_ Vars Vars VarID Term Term
          | Prim_ Primitive
          | Mark_ (Maybe String) Term
    -- deriving (Show, Eq)
    deriving (Eq)


freeVars :: Term -> Vars
freeVars (Var_ x) = singleton x
freeVars (App_ xs _ _ _) = xs
freeVars (Lam_ xs _ _ _) = xs
freeVars (Let_ xs _ _ _ _) = xs
freeVars (Prim_ _) = noVars
freeVars (Mark_ _ t) = freeVars t

vars :: Term -> Vars
vars (Var_ x) = singleton x
vars (App_ _ xs _ _) = xs
vars (Lam_ _ xs _ _) = xs
vars (Let_ _ xs _ _ _) = xs
vars (Prim_ _) = noVars
vars (Mark_ _ t) = vars t


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


prim :: Primitive -> Term
prim = Prim_


mark :: Maybe String -> Term -> Term
mark = Mark_

mark' :: Term -> Term
mark' = mark Nothing

markS :: String -> Term -> Term
markS = Mark_ . Just


data AST = Var VarID
         | App Term Term
         | Lam VarID Term
         | Let VarID Term Term
         | Prim Primitive
         | Mark (Maybe String) Term


ast :: Term -> AST
ast (Var_ x) = Var x
ast (App_ _ _ l r) = App l r
ast (Lam_ _ _ x m) = Lam x m
ast (Let_ _ _ x e m) = Let x e m
ast (Prim_ p) = Prim p
ast (Mark_ s t) = Mark s t



noVars :: Vars
noVars = S.empty

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


data Primitive = Num Int
               | NumOp (Int -> Int) PrimRep
               | NumBinOp (Int -> Int -> Int) PrimRep


type PrimRep = String


instance Eq Primitive where
    Num x == Num y = x == y
    NumOp _ r1 == NumOp _ r2 = r1 == r2
    NumBinOp _ r1 == NumBinOp _ r2 = r1 == r2
    _ == _ = False

instance Show Primitive where
    show (Num n) = show n
    show (NumOp _ r) = r
    show (NumBinOp _ r) = r

arity :: Primitive -> Int
arity (NumOp _ _) = 1
arity (NumBinOp _ _) = 2
arity _ = 0


primrep :: Primitive -> PrimRep
primrep (Num n) = show n
primrep (NumOp _ r) = r
primrep (NumBinOp _ r) = r


combineRep :: PrimRep -> PrimRep -> PrimRep
combineRep a b = a ++ ' ' : b

num :: Int -> Primitive
num = Num


