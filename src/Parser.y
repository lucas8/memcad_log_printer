
{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parser (
  parseExpr,
  parseTokens,
) where

import Lexer
import Syntax

import Control.Monad.Except

}

%tokentype { Token }

%token
    '==' { Tok1 }
    A    { Tok2 }

%monad { Except String } { (>>=) } { return }
%error { parseError }

%name entry
%%

Entry : A '==' A { AEq 0 [] }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: String -> Either String AST
parseExpr input = runExcept $ do
  let tokenStream = scanTokens input
  entry tokenStream

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . return . scanTokens

}

