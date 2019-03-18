
module Syntax where

import qualified Data.Map as M
import           Data.Map (Map)


--     _    ____ _____ 
--    / \  / ___|_   _|
--   / _ \ \___ \ | |  
--  / ___ \ ___) || |  
-- /_/   \_\____/ |_|  
--                     

data Variable = Var { var_name :: String
                    , var_id   :: Integer
                    , var_type :: String
                    } deriving (Eq,Show,Ord)

data Predicate = Pred { pred_name :: String
                      , pred_a1   :: [String]
                      , pred_a2   :: [String]
                      , pred_a3   :: [String]
                      } deriving (Eq,Show)

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


--   ____                 _     
--  / ___|_ __ __ _ _ __ | |__  
-- | |  _| '__/ _` | '_ \| '_ \ 
-- | |_| | | | (_| | |_) | | | |
--  \____|_|  \__,_| .__/|_| |_|
--                 |_|          


data Constr = CEq Expr | CInEq Expr | CEGe Expr Int | CEEq Expr Int
              deriving (Eq,Show)
data Output = ONode Node
            | OPred Predicate
            | OSeg Predicate Predicate Node
            | OConstraint Constr
              deriving (Eq,Show)

data Graph = Graph { gr_vars     :: [Variable]
                   , gr_varsVals :: Map String Node
                   , gr_links    :: Map Node (Map Int [Output]) -- Outputs for every offset
                   } deriving (Eq,Show) 



--   ____                 _       ____        _ _     _           
--  / ___|_ __ __ _ _ __ | |__   | __ ) _   _(_) | __| | ___ _ __ 
-- | |  _| '__/ _` | '_ \| '_ \  |  _ \| | | | | |/ _` |/ _ \ '__|
-- | |_| | | | (_| | |_) | | | | | |_) | |_| | | | (_| |  __/ |   
--  \____|_|  \__,_| .__/|_| |_| |____/ \__,_|_|_|\__,_|\___|_|   
--                 |_|                                            

addOutput :: Node -> Offset -> Output -> Graph -> Graph
addOutput nd off out (Graph vars vals links) =
    Graph vars vals
        $ M.insertWith (M.unionWith (++))
                       nd
                       (M.singleton off [out])
                       links

exprNodes :: Expr -> [Node] -> [Node]
exprNodes (ECst _)      = id
exprNodes (ENode nd)    = (nd :)
exprNodes (EMult e1 e2) = exprNodes e1 . exprNodes e2
exprNodes (EPlus e1 e2) = exprNodes e1 . exprNodes e2

addToGraph :: AST -> Graph -> Graph
addToGraph (AVar v nd) = \(Graph vars vals links) ->
    Graph (v : vars) (M.insert (var_name v) nd vals) links
addToGraph (ALink src off _ dst) =
    addOutput src off $ ONode dst
addToGraph (APred src prd) =
    addOutput src 0 $ OPred prd
addToGraph (ASeg src pr1 pr2 dst) =
    addOutput src 0 $ OSeg pr1 pr2 dst
addToGraph (AIneq i nodes) = \gr ->
    foldr (\src -> addOutput src 0 $ OConstraint $ CInEq $ ECst i) gr nodes
addToGraph (AEq i nodes) = \gr ->
    foldr (\src -> addOutput src 0 $ OConstraint $ CEq $ ECst i) gr nodes
addToGraph (AExpr expr Eq cst) = \gr ->
    foldr (\src -> addOutput src 0 $ OConstraint $ CEEq expr cst) gr $ exprNodes expr []
addToGraph (AExpr expr Ge cst) = \gr ->
    foldr (\src -> addOutput src 0 $ OConstraint $ CEGe expr cst) gr $ exprNodes expr []

makeGraph :: [AST] -> Graph
makeGraph = foldr addToGraph $ Graph [] M.empty M.empty

