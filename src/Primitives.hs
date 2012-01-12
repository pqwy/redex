{-# LANGUAGE ViewPatterns, PatternGuards  #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Primitives
    ( prims, applyPrim )
    where


import Ast

op :: String -> (Int -> Int) -> (String, Primitive)
op s f = (s, NumOp f s)

binop :: String -> (Int -> Int -> Int) -> (String, Primitive)
binop s f = (s, NumBinOp f s)


prims :: [(String, Primitive)]
prims = [ binop "+" (+)
        , binop "-" (-)
        , binop "*" (*)
        , binop "/" div
        , binop "%" mod
        , op "~" negate
        ]


applyPrim :: Primitive -> [Term] -> (Maybe Term)

applyPrim p@(NumBinOp f _)
          [ (ast -> Prim (Num n)), (ast -> Prim (Num m)) ] =
    (Just . prim . num) (f n m)

applyPrim p@(NumOp f _) [ (ast -> Prim a@(Num n)) ] =
    (Just . prim . num ) (f n)

applyPrim p1 p2 = Nothing



