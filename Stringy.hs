{-# LANGUAGE ViewPatterns  #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Stringy
    ( showsLam, parseLambda, parseLambdaOrValidatePrefix
    ) where


import Ast
import Operations
import Primitives

import Data.Function

import Text.Parsec
import Text.Parsec.String

import Text.Parsec.Error ( Message(..), errorMessages )

import Control.Monad
import Control.Applicative hiding ( Alternative(..), many )

-- in {{{

bracketed :: Parser a -> Parser a
bracketed p = char '(' *> spaces *> p <* spaces <* char ')'
-- bracketed p = try (char '(' *> spaces) *> p <* spaces <* char ')'

nonWS :: Parser String
nonWS = many1 (noneOf " \t\n")


parseTerm, parseSingleTerm, parseLam, parseVar, parseLet :: Parser Term

parseTerm = foldl1 app <$> many1 (parseSingleTerm <* spaces <?> "term")

parseSingleTerm = bracketed parseTerm <|> parseLam <|> parseLet
              <|> parseNum <|> try parsePrimOp <|> parseVar

lambda = oneOf "\\|λ"

parseLam = do
    vars <- lambda *> spaces *> smallVar `sepEndBy1` spaces <* char '.'
    spaces *> (flip (foldr lam) vars <$> parseTerm)

parseVar = var <$> largeVar

parseNum = prim . num . read <$> many1 digit <?> "numeral"

parsePrimOp = nonWS >>= \tok ->
                case lookup tok prims of
                     Nothing -> fail "unknown op"
                     Just p  -> pure (prim p)

parseLet = flip (foldr (uncurry leet))
       <$> (try (string "let") *> spaces *> many1 (try (binder <* spaces)))
       -- <$> (try (string "let") *> spaces *> many1 (binder <* spaces))
       <*> parseTerm

    where binder = bracketed ((,) <$> largeVar <* eq <*> parseTerm)
    -- where binder = bracketed ((,) <$> try (largeVar <* eq) <*> parseTerm)
          eq = spaces *> string "=" *> spaces


smallVar, largeVar :: Parser VarID

-- smallVar = (:[]) <$> letter <?> "short variable"
smallVar = (:) <$> letter <*> (many digit <|> pure []) <?> "short variable"

largeVar = (:) <$> letter <*> many alphaNum <?> "variable"




parseLambda :: SourceName -> String -> Either ParseError Term
parseLambda n s = parse parseTerm n s

parseLambdaOrValidatePrefix :: SourceName -> String -> Either ParseError (Maybe Term)
parseLambdaOrValidatePrefix n s =

    case parseLambda n s of
         Left e | unexpectedEOF e -> Right Nothing
                | otherwise       -> Left e
         r                        -> Just <$> r

unexpectedEOF = any (== SysUnExpect "") . errorMessages


instance Read Term where
    readsPrec _ = either (const []) (:[]) . parse p "<literal>"
        where p = spaces *> ((,) <$> parseTerm <*> getInput)


-- }}}

-- out {{{

bracketIfComposite, showsLam :: Term -> ShowS

bracketIfComposite t@(ast -> Var _)    = showsLam t
bracketIfComposite t@(ast -> Prim _)   = showsLam t
bracketIfComposite t@(ast -> Mark _ _) = showsLam t
bracketIfComposite t                   = showParen True (showsLam t)


showsLam (ast -> Var x) = showString x

showsLam (ast -> App l r) =
    ( case ast l of
           App _ _ -> showsLam l
           _       -> bracketIfComposite l )
    . (' ' :) . bracketIfComposite r

showsLam (ast -> Lam x t) =
    ('|' :) . (x ++)
    . fix ( \f t -> case ast t of
                Lam x t' -> (x ++) . f t'
                _        -> ('.' :) . showsLam t ) t

showsLam t@(ast -> Let _ _ _) =
    ("let " ++)
    . fix ( \f t -> case ast t of
                Let x e t' -> binder x e . f t'
                _          -> showsLam t ) t

    where binder x e = showParen True ( ((x ++ " = ") ++) . showsLam e ) . (' ' :)

showsLam (ast -> Prim p) = showPrimRep (primrep p)
    where showPrimRep = (++)

showsLam (ast -> Mark Nothing t) = (" [ "++) . showsLam t . (" ] "++)
showsLam (ast -> Mark (Just s) t) =
    ((" [ " ++ s ++ ": ")++) . showsLam t . (" ] "++)


instance Show Term where
    showsPrec _ = showsLam

-- }}}


-- vim:set fdm=marker:
