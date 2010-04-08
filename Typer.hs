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


type Ident = String

data Type = TyVar Ident | Arrow Type Type | TyCon Ident [Type]
    deriving (Eq)


instance Show Type where
    show (TyVar x)     = x
    show (Arrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
    show (TyCon c [])  = c
    show (TyCon c ts)  = c ++ " " ++ intercalate " " (map show ts)



type Subst = [(Ident, Type)]

emptySubst = [] :: Subst

extend :: Ident -> Type -> Subst -> Subst
extend x t = ((x, t) :)

substitute :: Subst -> Type -> Type
substitute s ty@(TyVar x)  = maybe ty (substitute s) (x `lookup` s)
substitute s (Arrow t1 t2) = Arrow (substitute s t1) (substitute s t2)
substitute s (TyCon k ts)  = TyCon k (map (substitute s) ts)

extend' :: Ident -> Type -> (Type -> Type) -> Type -> Type
extend' x t s ty@(TyVar y) | x   == y  = extend' x t s t
                           | ty' == ty = ty
                           | otherwise = extend' x t s ty'
                    where ty' = s ty
extend' x t s (Arrow t1 t2) = Arrow (extend' x t s t1) (extend' x t s t2)
extend' x t s (TyCon k ts)  = TyCon k (map (extend' x t s) ts)


data TypeScheme = Scheme [Ident] Type

instance Show TypeScheme where
    show (Scheme [] t) = show t
    show (Scheme as t) = "forall " ++ intercalate " " as ++ ". " ++ show t


newtype T a = T (StateT Int (Either String) a)
    deriving (Functor, Monad, MonadError String)

runT :: T a -> Either String a
runT (T s) = s `evalStateT` 0

instance Applicative T where
    pure = return
    (<*>) = ap

($>) :: (Applicative f) => f a -> (a -> b) -> f b
infix 4 $>
a $> f = f <$> a


newTyVar :: T Type
newTyVar = TyVar <$> T (get >>= \x -> ("a_" ++ show x) <$ put (x+1))

newInstance :: TypeScheme -> T Type
newInstance (Scheme ids ty) =
    substitute <$> foldM (\s id -> newTyVar $> \v -> extend id v s)
                         emptySubst ids
               <*> pure ty


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
                                        ++ a ++ " = " ++ show u' )
         (t, u'@(TyVar _)) -> mgu u' t s

         ((Arrow t1 t2), (Arrow u1 u2)) -> (mgu t1 u1 >=> mgu t2 u2) s

         ((TyCon a as), (TyCon b bs)) | a == b -> foldM (flip id) s (zipWith mgu as bs)

         (t', u') -> throwError ( "cannot unify " ++ show t' ++ " with " ++ show u' )


tp :: TypeEnv -> Term -> Type -> Subst -> T Subst

tp env (ast -> Var x) ty s =
    case x `lookup` env of
         Nothing -> throwError ("undefined: " ++ x)
         Just u  -> newInstance u >>= \i -> mgu i ty s

tp env (ast -> Lam x e1) ty s =
    do a <- newTyVar
       b <- newTyVar
       (mgu ty (Arrow a b) >=>
           tp ((x, Scheme [] a) : env) e1 b) s
    
tp env (ast -> App e1 e2) ty s =
    do a <- newTyVar
       (tp env e1 (Arrow a ty) >=> tp env e2 a) s

-- tp env (ast -> Let x e1 e2) ty s =
--     do a  <- newTyVar
--        s1 <- tp env e1 a s
--        tp ((x, generalize env (s1 `substitute` a)) : env) e2 ty s1

tp env (ast -> Let x e1 e2) ty s =
    do a  <- newTyVar
       s1 <- tp ((x, Scheme [] a) : env) e1 a s
       tp ((x, generalize env (s1 `substitute` a)) : env)
          e2 ty s1


typeOf :: TypeEnv -> Term -> Either String TypeScheme
typeOf env expr = runT
    ( newTyVar >>= \a ->
        tp env expr a emptySubst $>
            generalize env . (`substitute` a) )



predefEnv = [ (tc, generalize [] t) | (tc, t) <- env ]
    where
        bool   = TyCon "Bool" []
        int    = TyCon "Int"  []
        list a = TyCon "List" [a]
        type_  = TyCon "T" []

        a = TyVar "t"

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



