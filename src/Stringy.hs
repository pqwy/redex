{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PackageImports  #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Stringy (
      showsAST, parseAST, tryParseAST
    , showsType, showsScheme
    , funn
) where

import Ast

import Data.Function
import Control.Category ( (<<<) )

import Text.ParserCombinators.Parsec hiding ( State(..) )
import Text.ParserCombinators.Parsec.Error ( Message(..), errorMessages )

import Control.Applicative hiding ( Alternative(..), many )
import "monads-fd" Control.Monad.State

import Data.Generics

import qualified Language.Haskell.TH.Quote  as QQ

-- in {{{

cleanSpaces :: Parser a -> Parser a
cleanSpaces p = spaces *> p <* spaces

bracketed :: Parser a -> Parser b -> Parser c -> Parser c
bracketed open close p = open *> cleanSpaces p <* close

paren'd :: Parser a -> Parser a
paren'd = bracketed (char '(') (char ')')

pIdent :: Parser Ident
pIdent = cleanSpaces ( ((ident.) . (:)) <$> letter <*> many alphaNum <?> "variable" )

lambda :: Parser ()
lambda = () <$ oneOf "\\|λ"

pTerm, pAtom, pLambda, pVar, pLet, pTermEND :: Parser AST

pVar  = var <$> pIdent

pTerm = foldl1 app <$> many1 ( pAtom <?> "term" )

pAtom = spaces *> ( paren'd pTerm <|> pLambda <|> pLet <|> pVar ) <* spaces -- closing space??

pLambda = do
    vars <- lambda *> many1 pIdent <* char '.'
    flip (foldr lam) vars <$> pTerm

pLet = flip (foldr (uncurry let_))
        <$> (try (string "let") *> many1 (try binder))
        <*> pTerm
  where
    binder = cleanSpaces ( paren'd ((,) <$> pIdent <* string "=" <*> pTerm) )

pTermEND = pTerm <* eof

parseAST :: SourceName -> String -> Either ParseError AST
parseAST n s = parse pTerm n s

tryParseAST :: SourceName -> String -> Either ParseError (Maybe AST)
tryParseAST n s = case parseAST n s of
                       Left e | unexpectedEOF e -> Right Nothing
                              | otherwise       -> Left e
                       ast                      -> Just <$> ast

unexpectedEOF :: ParseError -> Bool
unexpectedEOF = any unexpected . errorMessages
    where
        unexpected (SysUnExpect "") = True
        unexpected _                = False


instance Read AST where
    readsPrec _ = either (const []) (:[]) . parse p "<literal>" where
              p = (,) <$> pTermEND <*> getInput

funn :: QQ.QuasiQuoter
funn = QQ.QuasiQuoter (either (\e -> error $ "\n" ++ show e ++ "\n")
                              (QQ.dataToExpQ (\_ -> Nothing))
                              . parse pTermEND "Quoted term")
                      no no no
  where
    no = error $ "funn: quasiquoter defined only for expression contexts."

-- }}}

-- out lam {{{

bracket :: String -> String -> ShowS -> ShowS
bracket open close between = (open ++) <<< between <<< (close ++)

parens :: ShowS -> ShowS
parens = bracket "(" ")"

showsASTParens, showsAST :: ASTAnn f => Term f -> ShowS

showsASTParens t@(ast -> Var _) = showsAST t
showsASTParens t                = parens (showsAST t)

showsAST (ast -> Var x) = shows x

showsAST (ast -> App l r) =
    ( case ast l of
           App _ _ -> showsAST       l
           _       -> showsASTParens l )
    <<< (' ' :) <<< showsASTParens r

showsAST (ast -> Lam x t) =
    ('λ' :) <<< shows x <<<
    fix ( \f t -> case ast t of
            Lam x t' -> shows x <<< f t'
            _        -> ('.' :) <<< showsAST t ) t

showsAST t@(ast -> Let _ _ _) =
    ("let " ++) <<<
    fix ( \f t -> case ast t of
            Let x e t' -> binder x e <<< f t'
            _          -> showsAST t ) t
  where
    binder x e = parens ( shows x <<< (" = "++) <<< showsAST e ) <<< (' ' :)


instance (Data (Term f), ASTAnn f) => Show (Term f) where
    showsPrec _ = showsAST . cleanIdents

-- }}}

-- {{{ out ty

showsTypeParens, showsType :: Type -> ShowS

showsTypeParens t@(TyVar _)    = showsType t
showsTypeParens t@(TyCon _ []) = showsType t
showsTypeParens t              = parens (showsType t)

showsType (TyVar i) = shows i
showsType (Arrow t1 t2) =
    showsTypeParens t1 <<< (" -> " ++) <<<
    case t2 of
         (Arrow _ _) -> showsType t2
         _           -> showsTypeParens t2

showsType (TyCon c []) = (c ++)
showsType (TyCon c ts) =
    (c ++) <<< foldr (\s k -> (' ':) . s . k) id (map showsTypeParens ts)

showsScheme (Scheme [] t) = showsType t
showsScheme (Scheme as t) =
    ("forall " ++) <<<
        foldr (\a -> ((shows a <<< (' ':)).)) id as <<<
        (". " ++) <<< showsType t


instance Show Type where
    showsPrec _ = showsType . cleanIdents

instance Show TypeScheme where
    showsPrec _ = showsScheme . cleanIdents

-- }}}

-- {{{ ident render

data IdentRenderState =
    IRS { candidate, takenGen0, takenGen1 :: [Ident]
        , replaceMap                      :: [(Ident, Ident)]
        }

type Shw a = State IdentRenderState a

runShw :: State IdentRenderState c -> c
runShw sh = (fst . fix) (\ ~(_, s) -> runState sh
                                IRS { candidate  = [ ident [x] | x <- ['a'..] ]
                                    , takenGen1  = takenGen0 s
                                    , takenGen0  = []
                                    , replaceMap = [] } )

cleanIdentifier :: Ident -> Shw Ident
cleanIdentifier t@(ID _) =
        modify (\s -> s { takenGen0 = t : takenGen0 s }) >> return t
cleanIdentifier t@(IDD _ _) = do
    bundle <- get
    case t `lookup` replaceMap bundle of
         Just t' -> return t'
         Nothing -> do
             let (c0:cands) = dropWhile (`elem` takenGen1 bundle)
                                        (candidate bundle)
             put bundle { candidate  = cands
                        , replaceMap = (t, c0) : replaceMap bundle }
             return c0

cleanIdents :: (Data t) => t -> t
cleanIdents = runShw . everywhereM (mkM cleanIdentifier)

-- }}}

-- vim:set fdm=marker:
