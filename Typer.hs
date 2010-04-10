{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports  #-}

module Typer
    ( TypeEnv, tp, typeOf
    ) where

import Ast hiding ( unions )
import Stringy

import Data.List ( intercalate, (\\), union )
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
runT (T s) = s `evalStateT` (IDD "a" 0)

infix 4 $>
a $> f = f <$> a


newTyVar :: T Type
newTyVar = TyVar <$> T (get >>= \x@(IDD a n) -> x <$ put (IDD a (n+1)))

newInstance :: TypeScheme -> T Type
newInstance (Scheme ids ty) =
        foldM (\s id -> newTyVar $> \v -> extend id v s)
                emptySubst ids
            $> (`substitute` ty)


type TypeEnv = [(Ident, TypeScheme)]


tyVars :: Type -> [Ident]
tyVars (TyVar x)     = [x]
tyVars (Arrow t1 t2) = tyVars t1 `union` tyVars t2
tyVars (TyCon c ts)  = unions (map tyVars ts)

schemeTyVars (Scheme ids t) = tyVars t \\ ids

envTyVars = unions . map (schemeTyVars . snd)

unions :: (Eq a) => [[a]] -> [a]
unions = foldr union []


generalize :: TypeEnv -> Type -> TypeScheme
generalize e t = Scheme (tyVars t \\ envTyVars e) t

mgu :: Type -> Type -> Subst -> T Subst

mgu t u s = case (s `substitute` t, s `substitute` u) of

         ((TyVar a), (TyVar b)) | a == b -> return s
         ((TyVar a), u')        | not (a `elem` tyVars u') -> return (extend a u' s)
                                | otherwise ->
                        throwError ( "Occurs check: cannot construct the infinite type: "
                                        ++ show a ++ " = " ++ showRaw u' )
         (t, u'@(TyVar _)) -> mgu u' t s

         ((Arrow t1 t2), (Arrow u1 u2)) -> (mgu t1 u1 >=> mgu t2 u2) s

         ((TyCon a as), (TyCon b bs)) | a == b -> foldM (flip id) s (zipWith mgu as bs)

         (t', u') -> throwError ( "cannot unify " ++ showRaw t' ++ " with " ++ showRaw u' )


tp :: TypeEnv -> Term -> Type -> Subst -> T Subst

tp env (ast -> Var x) ty s =
    case x `lookup` env of
         Nothing -> throwError ("undefined: " ++ show x)
         Just u  -> newInstance u >>= \i -> mgu i ty s

tp env (ast -> Lam x e1) ty s =
    do a <- newTyVar
       b <- newTyVar
       (mgu ty (Arrow a b) >=>
           tp ((x, Scheme [] a) : env) e1 b) s
    
tp env (ast -> App e1 e2) ty s =
    do a <- newTyVar
       (tp env e1 (Arrow a ty) >=> tp env e2 a) s

tp env (ast -> Let x e1 e2) ty s =
    do a  <- newTyVar
--     s1 <- tp env e1 a s
       s1 <- tp ((x, Scheme [] a) : env) e1 a s
       tp ((x, generalize env (s1 `substitute` a)) : env)
          e2 ty s1


typeOf :: TypeEnv -> Term -> Either String TypeScheme
typeOf env expr = runT
    ( newTyVar >>= \a ->
        tp env expr a emptySubst $>
            generalize env . (`substitute` a) )


showRaw :: Type -> String
showRaw = (`showsType` "")




predefEnv = [ (ID tc, generalize [] t) | (tc, t) <- env ]
    where
        bool   = TyCon "Bool" []
        int    = TyCon "Int"  []
        list a = TyCon "List" [a]
        type_  = TyCon "T" []

        a = TyVar (ID "t")

        infixr 9 ~>
        (~>) = Arrow

        env = [ ( "true"      , bool                    )
              , ( "false"     , bool                    )
              , ( "if"        , bool ~> a ~> a ~> a     )
              , ( "zero"      , int                     )
              , ( "succ"      , int ~> int              )
              , ( "nil"       , list a                  )
              , ( "isNil"     , list a ~> bool          )
              , ( "cons"      , a ~> list a ~> list a   )
              , ( "isEmpty"   , list a ~> bool          )
              , ( "head"      , list a ~> a             )
              , ( "tail"      , list a ~> list a        )
              , ( "fix"       , (a ~> a) ~> a           )
              , ( "undefined" , a                       )
              , ( "bop"       , type_ ~> type_ ~> type_ )
              , ( "unop"      , type_ ~> type_          )
              , ( "con"       , type_                   )
              ]


letMap  = "let ( map = |fx.if (isNil x) nil (cons (f (head x)) (map f (tail x))) ) "
letZip  = "let ( zip = |fxy.if (isNil x) nil (if (isNil y) nil (cons (f (head x) (head y)) (zip f (tail x) (tail y)))) )"
letZip' = "let ( zip = fix (|zfxy.if (isNil x) nil (if (isNil y) nil (cons (f (head x) (head y)) (z f (tail x) (tail y))))) ) "


expn :: [Term]
expn = map read [ "|x.cons x nil" ]



