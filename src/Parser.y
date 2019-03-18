
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
    '&'   { TokAddress   }
    '@'   { TokAt        }
    ','   { TokComma     }
    '{'   { TokLAcc      }
    '}'   { TokRAcc      }
    '('   { TokLPar      }
    ')'   { TokRPar      }
    'N|'  { TokVal       }
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
%left '+'
%left '*'
%%

Entry : Edge Entry { $1 : $2 }
      | Edge       { $1 : [] }

-- TODO support parsing of predicate arguments
Pred : IDENT '(' '|' '|' ')' { Pred $1 [] [] [] }

VarList : '|' NUM '|' ',' VarList { $2 : $5 }
        | '|' NUM '|'             { $2 : [] }
        |                         {      [] }

EqList : '=' '|' NUM '|' EqList { $3 : $5 }
       | '=' '|' NUM '|'        { $3 : [] }

Expr : Expr '+' Expr { EPlus $1 $3            }
     | Expr '*' Expr { EMult $1 $3            }
     | NUM           { ECst  (fromInteger $1) }
     | 'N|' NUM '|'  { ENode $2               }

Edge : '&' IDENT '(' NUM ')' TYPE NUM
           { AVar (Var $2 $4 $6) $7 }
     | NUM '-' '{' NUM '}' '-' '>' NUM
           { ALink $1 0 (fromInteger $4) $8 }
     | NUM '{' '@' '{' NUM '}' '}' '-' '{' NUM '}' '-' '>' NUM
           { ALink $1 (fromInteger $5) (fromInteger $10) $14 }
     | NUM '=' '=' Pred '=' '=' '>'
           { APred $1 $4 }
     | NUM '=' Pred '=' '=' '=' Pred '=' '>' NUM
           { ASeg $1 $3 $7 $10 }
     | NUM '!=' '{' VarList '}'
           { AIneq (fromInteger $1) $4 }
     | NUM EqList
           { AEq (fromInteger $1) $2 }
     | Expr '=' NUM
           { AExpr $1 Eq $ fromInteger $3 }
     | Expr '>=' NUM
           { AExpr $1 Ge $ fromInteger $3 }

{

parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

parseExpr :: [Token] -> Either String [AST]
parseExpr = runExcept . entry

parseTokens :: String -> Either String [Token]
parseTokens = runExcept . return . scanTokens

}

