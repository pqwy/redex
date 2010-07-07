{-# LANGUAGE ViewPatterns, PatternGuards  #-}
{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DoRec  #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Graphs
    ( graph, plotIt )
    where

import Ast

import Data.Graph.Inductive

import Control.Monad
import Control.Monad.Fix
import "transformers" Control.Monad.Trans.State
--  import "transformers" Control.Monad.Identity

import Control.Applicative hiding ( Alternative(..) )
import Control.Arrow ( first, second )

import Data.GraphViz


data NodeType = NLam | NApp | NVar Ident
    deriving (Show, Eq)

data RelType = Fun | Arg | Formal | Body
    deriving (Show, Eq)


graph :: (DynGraph g) => Term -> g NodeType RelType
graph t = let (_, ns, es) = evalState (termGraph t) (0, []) in mkGraph ns es


type GProc = State (Int, [(Ident, Int)]) 


label :: GProc Int
label = gets fst <* modify (first succ)

extend :: Ident -> Int -> GProc a -> GProc a
extend x n a =
    modify (second ((x, n):)) *> a <* modify (second tail)

resolve :: Ident -> GProc (Maybe Int)
resolve x = gets (lookup x . snd)


termGraph :: Term -> GProc (Int, [LNode NodeType], [LEdge RelType])

termGraph (ast -> Lam x m) = do
    n             <- label
    nx            <- label
    (lab, ns, ls) <- extend x nx $ termGraph m

    let nodes = (nx, NVar x) : (n, NLam) : ns
        edges = (n, nx, Formal) : (n, lab, Body) : ls
    return ( n, nodes, edges )

termGraph (ast -> App a b) = do
    n                <- label
    (alab, ans, aes) <- termGraph a
    (blab, bns, bes) <- termGraph b

    let nodes = (n, NApp) : ans ++ bns
        edges = (n, alab, Fun) : (n, blab, Arg) : aes ++ bes
    return ( n, nodes, edges )

termGraph (ast -> Var x) =
    resolve x >>= \x' -> case x' of
            Nothing -> label >>= \n -> return (n, [(n, NVar x)], [])
            Just n  -> return (n, [], [])

termGraph (ast -> Let x e m) = do
    rec (labe, ens, ees) <-
            extend x labe $ termGraph e

    (labm, mns, mes) <-
        extend x labe $ termGraph m

    return (labm, ens ++ mns, ees ++ mes)



plotIt :: FilePath -> Term -> IO (Either String FilePath)
plotIt p t = runGraphviz dot Png p

    where gr = graph t :: Gr NodeType RelType
          dot = graphToDot True gr [] (const []) (const [])

