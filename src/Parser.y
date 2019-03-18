
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
    NUM   { TokNumber $$ }
    IDENT { TokIdent  $$ }
    TYPE  { TokType   $$ }
    '@'   { TokAt        }
    ','   { TokComma     }
    '{'   { TokLAcc      }
    '}'   { TokRAcc      }
    '('   { TokLPar      }
    ')'   { TokRPar      }
    '|'   { TokBar       }
    ':'   { TokColon     }
    '+'   { TokPlus      }
    '-'   { TokMinus     }
    '*'   { TokTimes     }
    '!='  { TokNeq       }
    '>='  { TokGe        }
    '>'   { TokArrow     }
    '='   { TokEq        }

%monad { Except String } { (>>=) } { return }
%error { parseError }

%name entry
%%

Entry : NUM '=' '=' '@' { AEq (fromInteger $1) [] }

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

