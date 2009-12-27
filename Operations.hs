{-# LANGUAGE ViewPatterns, PatternGuards  #-}
{-# LANGUAGE PackageImports, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Operations
    ( substitute, {- rotateLet,-} leet
    , alpha, {- alpha', -} lazyBeta, splicingBeta
    -- , openScopeLam, openScopeLet
    ) where

import Ast

import Data.Function ( fix )
import Data.Maybe
import qualified SimpleSet as S


-- tree xforms {{{

-- openScopeLam :: VarID -> Term -> Term -> (VarID, Term)
-- openScopeLam x m t2 | x `notFreeIn` t2 = (x, m)
--                     | otherwise = let x' = newName x (vars m)
--                                   in (x', alpha x x' m)
-- 
-- openScopeLet :: VarID -> Term -> Term -> Term -> (VarID, Term, Term)
-- openScopeLet x e m t2
--     | x `notFreeIn` t2 = (x, e, m)
--     | otherwise = let x' = newName x (vars e `S.union` vars m)
--                   in (x', alpha x x' e, alpha x x' m)
-- 

openScopeFor :: Term -> Term -> Term

openScopeFor t2 t@(ast -> Lam x m)
    | x `notFreeIn` t2 = t
    | otherwise = let x' = x `newNameIn` m in lam x' (alpha x x' m)

openScopeFor t2 t@(ast -> Let x e m)
    | x `notFreeIn` t2 = t
    | otherwise = let x' = newName x (vars e `S.union` vars m)
                  in fixLet x' (alpha x x' e) (alpha x x' m)

openScopeFor t2 t = t


-- XXX alpha conversions!!
substitute :: Term -> VarID -> Term -> Term
substitute s x t | x `notFreeIn` t = t

substitute s x (ast . openScopeFor s -> App a b) =
    app (substitute s x a) (substitute s x b)
substitute s x (ast . openScopeFor s -> Lam y m) =
    lam y (substitute s x m)
substitute s x (ast . openScopeFor s -> Let y e m) =
    fixLet y (substitute s x e) (substitute s x m)
substitute s x (ast . openScopeFor s -> Var _) = s


-- -- XXX alpha conversions!!
-- substitute :: Term -> VarID -> Term -> Term
-- substitute s x t | x `notFreeIn` t = t
-- 
-- substitute s x (ast -> Var _)     = s
-- substitute s x (ast -> Lam y m)   =
--     let (y', m') = openScopeLam y m s
--     in lam y' (substitute s x m')
-- 
-- substitute s x (ast -> App a b)   =
--     app (substitute s x a) (substitute s x b)
-- 
-- substitute s x (ast -> Let y e m) =
--     let (y', e', m') = openScopeLet y e m s
--     in fixLet y' (substitute s x e') (substitute s x m')
--     -- leet y (substitute s x e) (substitute s x m)
--     -- fixLet y (substitute s x e) (substitute s x m)


-- rotateLet :: Term -> Maybe Term
-- rotateLet t =
--     fix ( \f vs t k -> case ast t of
--             Let x e m -> f (x `S.insert` vs) m (k . leet x e)
--             Lam x m | not (x `S.elem` vs)            -> k m x
--                     | Just (x', m') <- alpha' x vs m -> k m' x'
--                     | otherwise                      -> k m x
--             _         -> Nothing )
--         S.empty t (\m x -> Just (lam x m))


-- leet = fixLet
leet :: VarID -> Term -> Term -> Term
leet x e m | x `notFreeIn` m = m

leet x e t = case ast (openScopeFor e t) of

        App a b              -> bifurcate app a b
        Let y e1 m           -> bifurcate (fixLet y) e1 m
        Lam y m              -> lam y (leet x e m)
        Var _ | x `freeIn` e -> fixLet x e t
              | otherwise    -> e

    where bifurcate f a b | not (x `freeIn` a) = f a (leet x e b)
                          | not (x `freeIn` b) = f (leet x e a) b
                          | otherwise = fixLet x e t

-- leet x e t = case ast t of
-- 
--         App a b    -> bifurcate app a b
-- 
--         Let y e1 m -> bifurcate (fixLet y') e1' m'
--             where (y', e1', m') = openScopeLet y e1 m e
--         -- Let y e1 m | y `notFreeIn` e -> bifurcate (fixLet y) e1 m
--         --            | otherwise ->
--         --                let y' = newName y (vars e1 `S.union` vars m)
--         --                in bifurcate (fixLet y') (alpha y y' e1) (alpha y y' m)
--                                                
--         -- Let y e1 m -> bifurcate (fixLet y) e1 m
-- --         Let y e1 m | y `notFreeIn` e -> bifurcate (fixLet y) e1 m
-- --                    | otherwise 
-- 
--         Lam y m -> lam y' (leet x e m')
--             where (y', m') = openScopeLam y m e
--         -- Lam y m | y `notFreeIn` e -> lam y (leet x e m)
--         --         | otherwise       -> let (y', m') = alpha' y m
--         --                              in lam y' (leet x e m')
--         -- Lam y m | y `notFreeIn` e            -> lam y (leet x e m)
--         --         | Just (y', m') <- alpha y m -> lam y' (leet x e m')
--         --         | otherwise                  -> lam "_" (leet x e m)
-- 
--         Var _ | x `freeIn` e -> fixLet x e t
--               | otherwise    -> e

--     where bifurcate f a b | not (x `freeIn` a) = f a (leet x e b)
--                           | not (x `freeIn` b) = f (leet x e a) b
--                           | otherwise = fixLet x e t
-- 

-- pushLet :: VarID -> Term -> Term -> Maybe Term
-- 
-- pushLet x e t | x `notFreeIn` t = Nothing
-- 
-- pushLet x e (ast -> App a b)    = bifurcate x e app a b
-- pushLet x e (ast -> Let y e1 m) = bifurcate x e (leet y) e1 m
-- pushLet x e (ast -> Lam y m)    = lam y <$> pushLet x e m
-- pushLet x e (ast -> Var _)      = pure e
-- 
-- 
-- bifurcate x e f a b | pa && pb = pure (leet x e (f a b))
--                     | pa = flip f b <$> (pushLet x e a)
--                     | pb = f a <$> (pushLet x e b)
-- 
--     where (pa, pb) = (x `freeIn` a, x `freeIn` b)

-- }}}


newName :: VarID -> Vars -> VarID
newName v vs = head [ y' | n <- [ 0 .. ]
                         , let y' = v ++ show n
                         , not (y' `S.elem` vs) ]

newNameIn :: VarID -> Term -> VarID
newNameIn v = newName v . vars


-- abvgd {{{

alpha :: VarID -> VarID -> Term -> Term
alpha x y t | x `notFreeIn` t = t
            | otherwise =
                case ast t of
                     App a b    -> app (alpha x y a) (alpha x y b)
                     Lam x' m   -> lam x' (alpha x y m)
                     Let x' e m -> fixLet x' (alpha x y e) (alpha x y m)
                     Var _      -> var y


-- XXX CRAP
-- alpha' :: VarID -> Term -> (VarID, Term)
-- alpha' x t | x `notFreeIn` t = (x, t)
--            | otherwise = (x', alpha x x' t)
-- 
--     where x' = x `newNameIn` t
-- 
-- alpha :: VarID -> Term -> Maybe (VarID, Term)
-- alpha x = alpha' x S.empty
-- 
-- alpha' :: VarID -> Vars -> Term -> Maybe (VarID, Term)
-- alpha' x exclude t | x `notFreeIn` t = Nothing
--                    | otherwise       = Just (y, go t)
-- 
--     where (y:_) = [ y' | n <- [0..], let y' = x ++ show n
--                        , not (y' `varIn` t || y' `S.elem` exclude) ]
-- 
--           go t | x `notFreeIn` t = t
--                | otherwise = case ast t of
--                     App a b    -> app (go a) (go b)
--                     Lam x' m   -> lam x' (go m)
--                     Let x' e m -> leet x' (go e) (go m)
--                     Var _      -> var y
-- 

splicingBeta, lazyBeta :: Term -> Term -> Term

splicingBeta (ast -> Lam x m) t = substitute t x m
splicingBeta _ _ = error "splicingBeta: lambda?"

lazyBeta (ast -> Lam x m) t
        | x `notFreeIn` t = leet x t m
        | otherwise = let x' = x `newNameIn` m
                      in leet x' t (alpha x x' m)
--         | otherwise       = let (x', m') = alpha' x m
--                             in leet x' t m'
        -- | Just (x', m') <- alpha x m = leet x' t m'
        -- | Just (x', m') <- alpha x m = leet x' t m'
        -- | otherwise                  = m
lazyBeta _ _ = error "lazyBeta: lambda?"

-- }}}


-- vim:set fdm=marker:
