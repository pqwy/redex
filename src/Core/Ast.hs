{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Core.Ast (
      Ident (..), ident, variateIdent
    , Term (..), Node (..), AST, ast
    , ASTAnn (..), toAST, var, app, lam, let_
    , Type (..), TypeScheme (..)
) where

import Data.Data
import Data.Functor.Identity

import Data.String ( IsString (..) )

-- {{{  Identifiers

data Ident = ID  String
           | IDD String Int
    deriving (Eq, Ord, Typeable, Data)

instance Show Ident where
    show (ID  x  ) = x
    show (IDD x n) = x ++ "__" ++ show n

ident :: String -> Ident
ident = ID

variateIdent :: Ident -> Ident
variateIdent (ID  x  ) = IDD x 0
variateIdent (IDD x n) = IDD x (succ n)

instance IsString Ident where fromString = ident

-- }}}

-- {{{  Terms

--
-- The deal with terms. There is a tension between the
-- a) need to decorate the term representation in certain parts;
-- b) for the decoration to remain abstract, i.e. views <-> smart ctors; and
-- c) avoid type proliferation.
-- ...which is why terms are mutually wrapped with an arbitrary functor.
--
data Term f = Term (f (Node f))

data Node f = Var Ident
            | App (Term f) (Term f)
            | Lam Ident (Term f)
            | Let Ident (Term f) (Term f)

--
-- This would be the initial algebra of Terms, then.
--
type AST = Term Identity

toAST :: Node Identity -> AST
toAST = Term . Identity

var :: Ident -> AST
var  = toAST . Var

app :: AST -> AST -> AST
app  = (toAST.) . App

lam :: Ident -> AST -> AST
lam  = (toAST.) . Lam

let_ :: Ident -> AST -> AST -> AST
let_ = ((toAST.).) . Let

class ASTAnn a where strip :: a (Node a) -> Node a

instance ASTAnn Identity where strip = runIdentity

ast :: ASTAnn f => Term f -> Node f
ast (Term fn) = strip fn


tc :: String -> TyCon
tc = mkTyCon3 "lambdashell" "Ast"

--
-- Morbid instances. Pretty impredicative, but fun :) .
--
instance Typeable1 f => Typeable (Term f) where
    typeOf _ = tc "Term" `mkTyConApp` [typeOf1 (undefined :: f ())]

instance Typeable1 f => Typeable (Node f) where
    typeOf _ = tc "Node" `mkTyConApp` [typeOf1 (undefined :: f ())]

-- These two are, naturally, undecidable.
deriving instance (Typeable (Term f), Data (f (Node f))) => Data (Term f)
deriving instance (Typeable (Node f), Data (Term f)) => Data (Node f)

deriving instance Typeable1 Identity
deriving instance Data a => Data (Identity a)

-- Much simpler instances. Maybe go with this?
--
--  data Term a = T a (Node a)
--      deriving (Typeable, Data)

--  data Node a = Var Ident
--              | App (Term a) (Term a)
--              | Lam Ident (Term a)
--              | Let Ident (Term a) (Term a)
--      deriving (Typeable, Data)

-- }}}

-- {{{ types

-- Like System T, monomorphic + type constructors.
data Type = TyVar Ident
          | Arrow Type Type
          | TyCon String [Type]
    deriving (Eq, Typeable, Data)

-- Hindley-Milner scheme - single level of universal quantification.
data TypeScheme = Scheme [Ident] Type
    deriving (Eq, Typeable, Data)

-- }}}

