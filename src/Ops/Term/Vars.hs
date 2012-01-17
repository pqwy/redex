{-# LANGUAGE DeriveDataTypeable #-}

module Ops.Term.Vars where

import Core.Ast
import Internal.SimpleSet
import Internal.Random

import Data.Data


data Vars a =
    Vars { fv, v :: Set Ident
         , term  :: a
         } deriving (Data, Typeable)

type VAst = Term Vars

instance ASTAnn Vars where strip = term

free, vars :: VAst -> Set Ident
free (Term vars') = fv vars'
vars (Term vars') = v  vars'

annotate :: AST -> VAst
annotate = tpara 

--  mkVAst :: TermBuilder Vars
--  mkVAst = termBuilder $ (\f t -> uncurry Vars (f t) t) $ \t ->
--      case t of
--           Var i       -> ( singleton i, singleton i )
--           App e1 e2   -> ( free e1 |+| free e2, vars e1 |+| vars e2 )
--           Lam i e     -> ( i `remove` free e, i `insert` vars e )
--           Let i e1 e2 -> ( i `remove` (free e1 |+| free e2)
--                          , i `insert` (vars e1 |+| vars e2) )


