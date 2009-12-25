{-# LANGUAGE ViewPatterns  #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Stringy
    ( showsLam, parseLambda, parseLambdaOrValidatePrefix
    ) where


import Ast

import Text.Parsec
import Text.Parsec.String

import Text.Parsec.Error ( Message(..), errorMessages )

import Control.Monad
import Control.Applicative hiding ( Alternative(..), many )



bracketed :: Parser a -> Parser a
bracketed p = char '(' *> spaces *> p <* spaces <* char ')'


parseTerm, parseSingleTerm, parseLam, parseVar, parseLet :: Parser Term

parseTerm = foldl1 app <$> many1 (parseSingleTerm <* spaces <?> "term")

parseSingleTerm = bracketed parseTerm <|> parseLam <|> parseLet <|> parseVar

lambda = oneOf "\\|λ"

parseLam = do
    vars <- lambda *> spaces *> smallVar `sepEndBy1` spaces <* char '.'
    spaces *> (flip (foldr lam) vars <$> parseTerm)

parseVar = var <$> largeVar

parseLet = uncurry leet
       <$> (string "let" *> spaces *> binder <* spaces)
       <*> parseTerm

    where binder = bracketed ((,) <$> largeVar <* eq <*> parseTerm)
          eq = spaces *> string "=" *> spaces


smallVar, largeVar :: Parser VarID

smallVar = (:[]) <$> letter <?> "one-letter variable"

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
    readsPrec _ = either (const []) (:[]) . parse parsePlus "<literal>"
        where parsePlus = spaces *> ((,) <$> parseTerm <*> getInput)



bracketComposite, showsLam :: Term -> ShowS

bracketComposite t@(ast -> Var _) = showsLam t
bracketComposite t                = showParen True (showsLam t)


showsLam (ast -> Var x) = showString x

showsLam (ast -> App l r) = leftFun l . showChar ' ' . bracketComposite r
    where leftFun (ast -> Lam _ _) = bracketComposite l
          leftFun _                = showsLam l

showsLam (ast -> Lam x t) = showChar '\\' . showString x . run t
    where run (ast -> Lam x t') = showString x . run t
          run t                 = showChar '.' . showsLam t


instance Show Term where
    showsPrec _ = showsLam
