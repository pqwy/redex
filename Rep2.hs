module Rep where


import SimpleSet


type VarID = String


data Term = Var_ VarID | App_ (Set VarID) Term Term | Lam_ (Set VarID) VarID Term
    deriving (Show, Eq)

data BreadCrumb = BcAppL Term | BcAppR Term | BcLam VarID
    deriving (Show, Eq)

newtype Lambda = Lambda ([BreadCrumb], Term)
    deriving (Show, Eq)

data LambdaRep = Var VarID | App Lambda Lambda | Lam VarID Lambda
    deriving (Show, Eq)


freeVars :: Term -> Set VarID
freeVars (Var_ x)      = singleton x
freeVars (App_ xs _ _) = xs
freeVars (Lam_ xs _ _) = xs


var :: VarID -> Term
var = Var_

app :: Term -> Term -> Term
app a b = App_ (freeVars a `union` freeVars b) a b

lam :: VarID -> Term -> Term
lam x m = Lam_ (x `remove` freeVars m) x m


rep :: Lambda -> LambdaRep
rep (Lambda (_, Var_ x)) = Var x
rep (Lambda (bcs, App_ _ t1 t2)) =
    Lambda (BcAppL t2 : bcs, t1) `App` Lambda (BcAppR t1 : bcs, t2)
rep (Lambda (bcs, Lam_ _ x m)) = Lam x (Lambda (BcLam x : bcs, m))




