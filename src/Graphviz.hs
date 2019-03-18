{-# LANGUAGE OverloadedStrings #-}

module Graphviz where

import           Syntax
import           Data.Text.Lazy.Builder
import           Data.Monoid
import qualified Data.Map               as M
import           Data.Maybe

wrapper :: Builder -> Builder
wrapper gr = "digraph structs {\n" <> gr <> "}\n"

node :: Node -> Builder
node nd = "node" <> (fromString $ show nd)

var :: Variable -> Builder
var (Var _ i _) = "var" <> (fromString $ show i)

mkVar :: Variable -> Builder
mkVar v@(Var name i tp) = var v <> " [label=\""
                       <> "<id> "     <> (fromString $ show i)
                       <> " |{ <nm> " <> fromString name
                       <> " | <tp> "  <> fromString tp
                       <> " }\"];\n"

-- TODO support arguments
showPred :: Predicate -> Builder
showPred (Pred name _ _ _) = fromString name <> "(||)"

showExpr :: Expr -> Builder
showExpr (ECst i)      = fromString $ show i
showExpr (ENode nd)    = "N" <> (fromString $ show nd)
showExpr (EPlus e1 e2) = showExpr e1 <> " + " <> showExpr e2
showExpr (EMult e1 e2) = "(" <> showExpr e1 <> " * " <> showExpr e2 <> ")"

showConstr :: Node -> Constr -> Builder
showConstr nd (CEq e)    = "N" <> (fromString $ show nd) <>  " = " <> showExpr e
showConstr nd (CInEq e)  = "N" <> (fromString $ show nd) <> " != " <> showExpr e
showConstr _  (CEGe e i) = showExpr e <> " >= " <> (fromString $ show i)
showConstr _  (CEEq e i) = showExpr e <>  " = " <> (fromString $ show i)

mkNode :: Graph -> Node -> Builder
mkNode (Graph _ _ links) nd = node nd <> " [label=\""
                           <> "<id> " <> (fromString $ show nd)
                           <> "|{" <> offsets <> "}];\n"
 where outputs = M.lookup nd links
       mx      = fromMaybe 0 $ fst <$> (outputs >>= M.lookupMax)
       offsets = foldr1 (\x y -> x <> " | " <> y) $ map mkOff [0,4..mx]

       mkOutput :: Output -> Builder
       mkOutput (OPred prd)          = showPred prd         <> "&#92;n"
       mkOutput (OConstraint constr) = showConstr nd constr <> "&#92;n"
       mkOutput _                    = ""

       mkOffset :: [Output] -> Builder
       mkOffset = foldr (\out acc -> mkOutput out <> acc) ""

       mkOff :: Int -> Builder
       mkOff n = "<o" <> (fromString $ show n) <> "> "
              <> (mkOffset $ fromMaybe [] $ outputs >>= M.lookup n)
                       

