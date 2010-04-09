module Rep where


import SimpleSet


type Ident = String


data Term = Var_ Ident | App_ (Set Ident) Term Term | Lam_ (Set Ident) Ident Term
    deriving (Show, Eq)

data BreadCrumb = BcAppL Term | BcAppR Term | BcLam Ident
    deriving (Show, Eq)

newtype Lambda = Lambda ([BreadCrumb], Term)
    deriving (Show, Eq)

data LambdaRep = Var Ident | App Lambda Lambda | Lam Ident Lambda
    deriving (Show, Eq)


freeVars :: Term -> Set Ident
freeVars (Var_ x)      = singleton x
freeVars (App_ xs _ _) = xs
freeVars (Lam_ xs _ _) = xs


var :: Ident -> Term
var = Var_

app :: Term -> Term -> Term
app a b = App_ (freeVars a `union` freeVars b) a b

lam :: Ident -> Term -> Term
lam x m = Lam_ (x `remove` freeVars m) x m


rep :: Lambda -> LambdaRep
rep (Lambda (_, Var_ x)) = Var x
rep (Lambda (bcs, App_ _ t1 t2)) =
    Lambda (BcAppL t2 : bcs, t1) `App` Lambda (BcAppR t1 : bcs, t2)
rep (Lambda (bcs, Lam_ _ x m)) = Lam x (Lambda (BcLam x : bcs, m))




