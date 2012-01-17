{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Repr.TH (
    term
) where

import Core.Ast          ( AST, Ident (..) )
import Core.Transmogrify ( Builder (..), tfold, bAst )
import Repr.Stringy      ( parseAST )

import qualified Language.Haskell.TH.Quote as QQ

import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax ( Lift (..) )


instance Lift AST where lift = liftAST

liftAST :: AST -> TH.ExpQ
liftAST = tfold id b
  where
    b = Builder
        ( \id       -> [| var bAst $(i id) |] )
        ( \e1 e2    -> [| app bAst |] `apps` [ e1, e2 ] )
        ( \id e     -> [| lam bAst $(i id) |] `TH.appE` e )
        ( \id e1 e2 -> [| le7 bAst $(i id) |] `apps` [ e1, e2 ] )

    i (Id name) = [| Id name |]

    apps = foldl TH.appE

term :: QQ.QuasiQuoter
term = QQ.QuasiQuoter (either (\e -> error $ "\n" ++ show e ++ "\n")
                              liftAST
                              . parseAST "Quoted term")
                      no no no
  where
    no = error $ "term: quasiquoter defined only for expression contexts."

