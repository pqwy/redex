{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE PackageImports  #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Stringy
    ( showsLam, parseLambda, parseLambdaOrValidatePrefix
    , showsType, showsScheme
    , funn
    ) where


import Ast
import Operations
--  import Primitives

import Data.Function
import Control.Category ( (>>>), (<<<) )

import Text.ParserCombinators.Parsec hiding ( State(..) )
-- import Text.Parsec.String

import Text.ParserCombinators.Parsec.Error ( Message(..), errorMessages )

import Control.Monad
import Control.Applicative hiding ( Alternative(..), many )
import "monads-fd" Control.Monad.State

import Data.Data
import Data.Generics

import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Quote  as QQ

-- in {{{

surrounded :: Parser a -> Parser b -> Parser c -> Parser c
surrounded open close p = open *> spaces *> p <* spaces <* close
-- surrounded p = try (open *> spaces) *> p <* spaces <* close

bracketed :: Parser a -> Parser a
bracketed = surrounded (char '(') (char ')')

nonWS :: Parser String
nonWS = many1 (noneOf " \t\n")


parseTerm, parseSingleTerm, parseLam, parseVar, parseLet :: Parser Term

parseTerm = spaces *>
            (foldl1 app <$> many1 (parseSingleTerm <* spaces <?> "term"))

parseSingleTerm = bracketed parseTerm <|> parseLam <|> parseLet
--                <|> parseNum <|> try parsePrimOp
              <|> parseVar

lambda = oneOf "\\|λ"

parseLam = do
    vars <- lambda *> spaces *> smallVar `sepEndBy1` spaces <* char '.'
    spaces *> (flip (foldr lam) vars <$> parseTerm)

parseVar = var <$> largeVar

--  parseNum = prim . num . read <$> many1 digit <?> "numeral"

--  parsePrimOp = nonWS >>= \tok ->
--                  case lookup tok prims of
--                       Nothing -> fail "unknown op"
--                       Just p  -> pure (prim p)

parseLet = flip (foldr (uncurry leet))
       <$> (try (string "let") *> spaces *> many1 (try (binder <* spaces)))
       -- <$> (try (string "let") *> spaces *> many1 (binder <* spaces))
       <*> parseTerm

    where binder = bracketed ((,) <$> largeVar <* eq <*> parseTerm)
    -- where binder = bracketed ((,) <$> try (largeVar <* eq) <*> parseTerm)
          eq = spaces *> string "=" *> spaces


smallVar, largeVar :: Parser Ident

-- smallVar = (:[]) <$> letter <?> "short variable"
smallVar = (\c -> ident . (c:)) <$> letter <*> (many digit <|> pure [])
                                <?> "short variable"

largeVar = (\c -> ident . (c:)) <$> letter <*> many alphaNum <?> "variable"




parseLambda :: SourceName -> String -> Either ParseError Term
parseLambda n s = parse parseTerm n s

parseLambdaOrValidatePrefix :: SourceName -> String -> Either ParseError (Maybe Term)
parseLambdaOrValidatePrefix n s =

    case parseLambda n s of
         Left e | unexpectedEOF e -> Right Nothing
                | otherwise       -> Left e
         r                        -> Just <$> r

unexpectedEOF = any unexpected . errorMessages
    where
        unexpected (SysUnExpect "") = True
        unexpected _                = False


instance Read Term where
    readsPrec _ = either (const []) (:[]) . parse p "<literal>"
        where p = ((,) <$> parseTerm <*> getInput)

instance TH.Lift Term where
    lift = QQ.dataToQa patch TH.litE (foldl TH.appE) (const Nothing)
      where
        patch n | isTermCtor (TH.nameBase n) = TH.varE n
                | otherwise                  = TH.conE n

funn :: QQ.QuasiQuoter
funn = qqlift "funn" parseTerm


qqlift :: TH.Lift t => String -> Parser t -> QQ.QuasiQuoter
qqlift name p =
    QQ.QuasiQuoter
        (either (\e -> error $ "\n" ++ show e ++ "\n") TH.lift
            . parse (p <* eof) "Quoted term")
        e e e
  where
    e = error $ name ++ ": quasiquoter defined only for expression contexts."

-- }}}

-- out lam {{{

surround :: String -> String -> ShowS -> ShowS
surround open close between = (open ++) <<< between <<< (close ++)

parens :: ShowS -> ShowS
parens = surround "(" ")"

group, showsLam :: Term -> ShowS

group t@(ast -> Var _)    = showsLam t
--  group t@(ast -> Prim _)   = showsLam t
group t@(ast -> Mark _ _) = showsLam t
group t                   = parens (showsLam t)


showsLam (ast -> Var x) = shows x

showsLam (ast -> App l r) =
    ( case ast l of
           App _ _ -> showsLam l
           _       -> group l )
    <<< (' ' :) <<< group r

showsLam (ast -> Lam x t) =
    ('λ' :) <<< shows x <<<
    fix ( \f t -> case ast t of
            Lam x t' -> shows x <<< f t'
            _        -> ('.' :) <<< showsLam t ) t

showsLam t@(ast -> Let _ _ _) =
    ("let " ++) <<<
    fix ( \f t -> case ast t of
            Let x e t' -> binder x e <<< f t'
            _          -> showsLam t ) t

    where binder x e = parens ( shows x <<< (" = "++) <<< showsLam e ) <<< (' ' :)

--  showsLam (ast -> Prim p) = showPrimRep (primrep p)
--      where showPrimRep = (++)

showsLam (ast -> Mark tag t) = surround open "]" (showsLam t)
  where
    open = maybe " [ " (("[ "++) . (++ ": ")) tag


instance Show Term where
    showsPrec _ = showsLam . cleanIdents

-- }}}

-- {{{ out ty

brk, showsType :: Type -> ShowS

brk t@(TyVar _)    = showsType t
brk t@(TyCon _ []) = showsType t
brk t              = parens (showsType t)

showsType (TyVar i) = shows i
showsType (Arrow t1 t2) =
    brk t1 <<< (" -> " ++) <<<
    case t2 of
         (Arrow _ _) -> showsType t2
         _           -> brk t2

showsType (TyCon c []) = (c ++)
showsType (TyCon c ts) =
    (c ++) <<< foldr (\s k -> (' ':) . s . k) id (map brk ts)

instance Show Type where
    showsPrec _ = showsType . cleanIdents

showsScheme (Scheme [] t) = showsType t
showsScheme (Scheme as t) =
    ("forall " ++) <<<
        foldr (\a -> ((shows a <<< (' ':)).)) id as <<<
        (". " ++) <<< showsType t

instance Show TypeScheme where
    showsPrec _ = showsScheme . cleanIdents

-- }}}

-- {{{ ident render

data IdentRenderState = IRS { candidate, takenGen0, takenGen1 :: [Ident]
                            , replaceMap :: [(Ident, Ident)] }
type Shw a = State IdentRenderState a

runShw sh = (fst . fix) (\ ~(_, s) -> runState sh
                                IRS { candidate = [ ident [x] | x <- ['a'..] ]
                                    , takenGen1 = takenGen0 s
                                    , takenGen0 = [], replaceMap = [] } )


cleanIdentifier :: Ident -> Shw Ident
cleanIdentifier t@(ID _) =
        modify (\s -> s { takenGen0 = t : takenGen0 s }) >> return t

cleanIdentifier t@(IDD x n) = do
    bundle <- get
    case t `lookup` replaceMap bundle of
         Just t' -> return t'
         Nothing -> do
             let (c0:cands) = dropWhile (`elem` takenGen1 bundle)
                                        (candidate bundle)
             put (bundle { candidate = cands
                         , replaceMap = (t, c0) : replaceMap bundle })
             return c0

cleanIdents :: (Data t) => t -> t
cleanIdents = runShw . everywhereM (mkM cleanIdentifier)


-- }}}

-- vim:set fdm=marker:
