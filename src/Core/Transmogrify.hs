{-# LANGUAGE ViewPatterns  #-}

module Core.Transmogrify where

import Core.Ast
import Data.Functor.Identity


-- Make them.
--

data Builder a b = Builder {
      var :: Ident -> b
    , app :: a     -> a -> b
    , lam :: Ident -> a -> b
    , le7 :: Ident -> a -> a -> b
    }

instance Functor (Builder a) where
    fmap f b = Builder (f . var b) ((f.) . app b)
                       ((f.) . lam b) (((f.).) . le7 b)

bNode :: Builder (Term f) (Node f)
bNode = Builder Var App Lam Let

bAst :: Builder AST AST
bAst = (Term . Identity) `fmap` bNode

-- Change them.
-- 
tpara :: ASTAnn f => (Term f -> b -> a) -> Builder a b -> Term f -> a
tpara f b t@(ast -> Var i       ) = f t (var b i)
tpara f b t@(ast -> App e1 e2   ) = f t (app b (tpara f b e1) (tpara f b e2))
tpara f b t@(ast -> Lam i  e    ) = f t (lam b i (tpara f b e))
tpara f b t@(ast -> Let i  e1 e2) = f t (le7 b i (tpara f b e1) (tpara f b e2))
tpara _ _ _ = error "Core.Transmogrify.tpara: missing ctor."

tfold :: ASTAnn f => (b -> a) -> Builder a b -> Term f -> a
tfold = tpara . const

tmap :: ASTAnn f => (Term f -> Node g -> Term g) -> Term f -> Term g
tmap f = tpara f bNode

