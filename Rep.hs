module Rep where


import SimpleSet ( Set, singleton, union, remove )


type Ident = String
type SpliceID = String

data Lam1 = Var Ident | App Lam1 Lam1 | Lam Ident Lam1 | Splice SpliceID
    deriving (Eq, Show)

data Lam2 = Lam2 { freeVars :: Set Ident, term :: Lam1 }
    deriving (Eq, Show)

data BreadCrumb = BcAppLeft Lam2 | BcAppRight Lam2 | BcLam Ident
    deriving (Eq, Show)

newtype Lambda = Lambda ([BreadCrumb], Lam2)
    deriving (Eq, Show)


zip :: Lam2 -> Lambda
zip l = Lambda ([], l)


var :: Ident -> Lam2
var x = Lam2 { freeVars = singleton x, term = Var x }

app :: Lam2 -> Lam2 -> Lam2
app f a = Lam2 { freeVars = freeVars f `union` freeVars a
               , term = App (term f) (term a) }

lam :: Ident -> Lam2 -> Lam2
lam x m = Lam2 { freeVars = x `remove` freeVars m
               , term = Lam x (term m) }



up :: Lambda -> Maybe Lambda
up (Lambda ([], _)) = Nothing
up (Lambda (BcAppLeft r  : bcs, l)) = Just $ Lambda (bcs, app l r)
up (Lambda (BcAppRight l : bcs, r)) = Just $ Lambda (bcs, app l r)
up (Lambda (BcLam v      : bcs, m)) = Just $ Lambda (bcs, lam v m)


root :: Lambda -> Lam2
root (Lambda (_, l)) = l


downLam :: Lambda -> Lambda
downLam (Lambda (bcs, Lam2 { term = Lam x m })) = Lambda (BcLam x : bcs, m)
downLam _ = error "downLam: not a lambda term."



