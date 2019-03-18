
{
module Lexer(Token(..), scanTokens) where

import Syntax

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-
    $eol     ;
    $white+  ;
    $digit   { \s -> Tok1 }
    $alpha   { \s -> Tok2 }

{
data Token = Tok1 | Tok2
           deriving (Eq, Show)

scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}

