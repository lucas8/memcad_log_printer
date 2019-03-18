
{
module Lexer(Token(..), scanTokens) where

}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z_\%\#]
$eol   = [\n]

tokens :-
    $eol        ;
    $white+     ;
    ": ".*" ->" { TokType . readType }
    "N|"        { const TokVal       }
    $digit+     { TokNumber . read   }
    $alpha+     { TokIdent           }
    \&          { const TokAddress   }
    \@          { const TokAt        }
    \,          { const TokComma     }
    \{          { const TokLAcc      }
    \}          { const TokRAcc      }
    \(          { const TokLPar      }
    \)          { const TokRPar      }
    \|          { const TokBar       }
    \:          { const TokColon     }
    \+          { const TokPlus      }
    \-          { const TokMinus     }
    \.          { const TokTimes     }
    "!="        { const TokNeq       }
    ">="        { const TokGe        }
    \>          { const TokArrow     }
    \=          { const TokEq        }

{
data Token = TokNumber Integer
           | TokIdent  String
           | TokType   String
           | TokAt | TokAddress
           | TokComma
           | TokVal
           | TokLAcc | TokRAcc
           | TokLPar | TokRPar
           | TokBar | TokArrow | TokColon
           | TokPlus | TokMinus | TokTimes
           | TokNeq | TokEq | TokGe
           deriving (Eq, Show)

readType :: String -> String
readType tp = drop 2 $ take (length tp - 3) tp

scanTokens :: String -> [Token]
scanTokens = alexScanTokens
}

