
module Syntax where

data Variable = Var { var_name :: String
                    , var_id   :: Integer
                    , var_type :: String
                    } deriving (Eq,Show)

data Predicate = Pred { pred_name :: String
                      , pred_a1   :: [String]
                      , pred_a2   :: [String]
                      , pred_a3   :: [String]
                      } deriving (Eq, Show)

type Node   = Integer
type Size   = Int
type Offset = Int
data Expr   = ECst Int
            | ENode Node
            | EMult Expr Expr
            | EPlus Expr Expr
            deriving (Eq,Show)
data Op = Eq | Ge deriving (Eq,Show)

data AST = AVar Variable Node
         | ALink Node Offset Size Node
         | APred Node Predicate
         | ASeg  Node Predicate Predicate Node
         | AIneq Int [Node]
         | AEq   Int [Node]
         | AExpr Expr Op Int
         deriving (Eq,Show)

