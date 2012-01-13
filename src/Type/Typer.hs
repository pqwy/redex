{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE OverloadedStrings  #-}

module Type.Typer
--      ( TypeEnv, tp, typeOf, predefEnv
--      ) where
where

import Prelude hiding ( elem )
import           Core.Ast
import           Core.Environment hiding ( lookup )
import qualified Core.Environment as E
import           Internal.SimpleSet
import           Internal.Random
import           Repr.Stringy

import Data.Monoid

--  import Data.List ( intercalate, (\\), union )
import "transformers" Control.Monad.Trans.State
import "monads-fd" Control.Monad.Error
import Control.Applicative


type Subst = [(Ident, Type)]

emptySubst = [] :: Subst

extend :: Ident -> Type -> Subst -> Subst
extend x t = ((x, t) :)

substitute :: Subst -> Type -> Type
substitute s ty@(TyVar x)  = maybe ty (substitute s) (x `lookup` s)
substitute s (Arrow t1 t2) = Arrow (substitute s t1) (substitute s t2)
substitute s (TyCon k ts)  = TyCon k (map (substitute s) ts)


newtype T a = T (StateT Ident (Either String) a)
    deriving (Functor, Monad, MonadError String, Applicative)

runT :: T a -> Either String a
runT (T s) = s `evalStateT` variateIdent (ident "a")


newTyVar :: T Type
newTyVar = TyVar <$> T (get >>= \x -> x <$ put (variateIdent x))

newInstance :: TypeScheme -> T Type
newInstance (Scheme ids ty) =
        foldM (\s id -> newTyVar $> \v -> extend id v s)
                emptySubst ids
            $> (`substitute` ty)


type TypeEnv = Environment TypeScheme

scheme :: Set Ident -> Type -> TypeScheme
scheme = Scheme . toList

scheme_ :: TypeScheme -> (Set Ident, Type)
scheme_ (Scheme ids t) = (fromList ids, t)

tyVars :: Type -> Set Ident
tyVars (TyVar x)     = singleton x
tyVars (Arrow t1 t2) = tyVars t1 `union` tyVars t2
tyVars (TyCon c ts)  = mconcat (map tyVars ts)

schemeTyVars :: TypeScheme -> Set Ident
schemeTyVars (scheme_ -> (ids, t)) = tyVars t \\ ids

envTyVars :: TypeEnv -> Set Ident
envTyVars = mconcat . map schemeTyVars . elems

generalize :: TypeEnv -> Type -> TypeScheme
generalize e t = scheme (tyVars t \\ envTyVars e) t

--
-- Simple unification of two type expressions, given substitution environment.
--
unify :: Type -> Type -> Subst -> T Subst
unify t u s = case (s `substitute` t, s `substitute` u) of

         (TyVar a, TyVar b) | a == b -> return s

         (TyVar a, u') | not (a `elem` tyVars u') -> return (extend a u' s)
                       | otherwise                -> throwError
                                    ( "Occurs check: cannot construct the infinite type: "
                                        ++ show a ++ " = " ++ showRaw u' )
         (t, u'@(TyVar _)) ->
             unify u' t s

         (Arrow t1 t2, Arrow u1 u2) ->
             (unify t1 u1 >=> unify t2 u2) s

         (TyCon a as, TyCon b bs) | a == b ->
             foldM (flip id) s (zipWith unify as bs)

         (t', u') ->
             throwError ( "cannot unify " ++ showRaw t' ++ " with " ++ showRaw u' )

showRaw :: Type -> String
showRaw = (`showsType` "")

--
-- Prove a term can be typed by a particular type, in a given type environment
-- and with a given unifier, producing the most general unifier satisfying this
-- along the way.
-- 
prove :: ASTAnn f => TypeEnv -> Term f -> Type -> Subst -> T Subst

prove env (ast -> Var x) ty s =
    case x `E.lookup` env of
         -- Nothing -> throwError ("undefined: " ++ show x)
         Nothing -> return s -- does not constrain ty: unknowns type as anything at all >:)
         Just u  -> newInstance u >>= \i -> unify i ty s

prove env (ast -> Lam x e1) ty s =
    do a <- newTyVar
       b <- newTyVar
       ( unify ty (Arrow a b) >=>
           prove (x --> Scheme [] a |+| env) e1 b ) s

prove env (ast -> App e1 e2) ty s =
    do a <- newTyVar
       ( prove env e1 (Arrow a ty) >=> prove env e2 a ) s

prove env (ast -> Let x e1 e2) ty s =
    do a  <- newTyVar
--     s1 <- prove env e1 a s  -- non-recursive let
       s1 <- prove (x --> Scheme [] a |+| env) e1 a s
       prove ( x --> generalize env (s1 `substitute` a) |+| env )
          e2 ty s1


--
-- Type inference: a matter of proving that a fresh type variable types the
-- term, and noting what the variable ended up unified with.
-- 
inferType :: ASTAnn f => TypeEnv -> Term f -> Either String TypeScheme
inferType env expr = runT
    ( newTyVar >>= \a ->
        prove env expr a emptySubst $>
            generalize env . (`substitute` a) )

predefEnv :: TypeEnv
predefEnv = fmap (generalize mempty) (mconcat env)
    where
        bool     = TyCon "Bool" []
        int      = TyCon "Int"  []
        list a   = TyCon "List" [a]
        pair a b = TyCon "Pair" [a, b]
        type_    = TyCon "T" []

        a = TyVar (ident "t")
        b = TyVar (ident "u")

        i = ident

        infixr 9 ~>
        (~>) = Arrow

        env = [ "true"      -->  bool
              , "false"     -->  bool
              , "if"        -->  bool ~> a ~> a ~> a
              , "zero"      -->  int
              , "succ"      -->  int ~> int
              , "nil"       -->  list a
              , "isNil"     -->  list a ~> bool
              , "cons"      -->  a ~> list a ~> list a
              , "isEmpty"   -->  list a ~> bool
              , "head"      -->  list a ~> a
              , "tail"      -->  list a ~> list a
              , "fix"       -->  (a ~> a) ~> a
              , "pair"      -->  a ~> b ~> pair a b
              , "fst"       -->  pair a b ~> a
              , "snd"       -->  pair a b ~> b
              , "undefined" -->  a
              , "bop"       -->  type_ ~> type_ ~> type_
              , "unop"      -->  type_ ~> type_
              , "con"       -->  type_
              ]

--  letMap  = "let ( map = |fx.if (isNil x) nil (cons (f (head x)) (map f (tail x))) ) "
--  letZip  = "let ( zip = |fxy.if (isNil x) nil (if (isNil y) nil (cons (f (head x) (head y)) (zip f (tail x) (tail y)))) )"
--  letZip' = "let ( zip = fix (|zfxy.if (isNil x) nil (if (isNil y) nil (cons (f (head x) (head y)) (z f (tail x) (tail y))))) ) "

