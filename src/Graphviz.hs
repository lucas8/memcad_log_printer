{-# LANGUAGE OverloadedStrings #-}

module Graphviz (graphToGraphviz) where

import           Syntax
import           Data.Text.Lazy.Builder
import           Data.Text.Lazy         (Text)
import           Data.Monoid
import qualified Data.Map               as M
import           Data.Maybe
import           Data.List
import           Control.Arrow

escape :: String -> String -> String
escape _     [] = []
escape toesc (h : t) = if h `elem` toesc then '\\' : h : escape toesc t
                                         else        h : escape toesc t

escLab :: String -> String
escLab = escape ['|', '{', '}', '[', ']']

wrapper :: Builder -> Builder
wrapper gr = "digraph structs {\nnode [shape=record]\n" <> gr <> "}\n"

node :: Node -> Builder
node nd = "node" <> (fromString $ show nd)

var :: Variable -> Builder
var (Var _ i _) = "var" <> (fromString $ show i)

mkVar :: Variable -> Builder
mkVar v@(Var name i tp) = var v <> " [label=\""
                       <> "<id> "     <> (fromString $ show i)
                       <> " |{ <nm> " <> fromString name
                       <> " | <tp> "  <> (fromString $ escLab tp)
                       <> " }\"];\n"

-- TODO support arguments
showPred :: Predicate -> Builder
showPred (Pred name _ _ _) = fromString name <> "(\\|\\|)"

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

mkNode :: M.Map Node (M.Map Int [Output]) -> Node -> Builder
mkNode links nd = node nd <> " [label=\""
               <> "<id> " <> (fromString $ show nd)
               <> "|{" <> offsets <> "}\"];\n"
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
                       
mkNodeEdges :: Graph -> Builder
mkNodeEdges = mconcat . map (uncurry mkEdges) . M.toList . gr_links

mkEdges :: Node -> M.Map Int [Output] -> Builder
mkEdges src = mconcat . map (uncurry $ mkOffEdges src) . M.toList

mkOffEdges :: Node -> Int -> [Output] -> Builder
mkOffEdges src off = mconcat . map (mkEdge src off)

mkEdge :: Node -> Int -> Output -> Builder
mkEdge src off (ONode dst) = node src <> ":o" <> (fromString $ show off)
                          <> " -> " <> node dst <> ";\n"
mkEdge src off (OSeg p1 p2 dst) = node src <> ":o" <> (fromString $ show off)
                               <> " -> " <> node dst
                               <> " [color = \"black:invis:black\", label=\""
                               <> showPred p1 <> " -- "
                               <> showPred p2 <> "\"];\n"
mkEdge _ _ _ = ""

mkVarEdge :: Variable -> Node -> Builder
mkVarEdge vr dst = var vr <> " -> " <> node dst <> ";\n"

mkVarEdges :: Graph -> Builder
mkVarEdges (Graph vars varVals _) = fromMaybe ""
                                  $ mconcat
                                  $ map ( id &&& (var_name >>> flip M.lookup varVals)
                                      >>> sequence
                                      >>> (fmap . uncurry) mkVarEdge)
                                  $ vars

wrapVars :: Builder -> Builder
wrapVars vars = "subgraph cluster_vars {\n"
             <> "label = \"Variables\";\n"
             <> vars
             <> "}\n"

listNodes :: Graph -> [Node]
listNodes (Graph _ _ links) = nub $ (map fst $ M.toList links)
                                 <> foldMap (foldMap $ mconcat . (fmap fromOutput)) links
 where fromOutput :: Output -> [Node]
       fromOutput (ONode nd)    = [nd]
       fromOutput (OSeg _ _ nd) = [nd]
       fromOutput _             = []

graphToGraphviz :: Graph -> Text
graphToGraphviz gr = toLazyText $ wrapper
                                $    wrapVars (mconcat $ map mkVar $ gr_vars gr)
                                  <> mkVarEdges gr
                                  <> (mconcat $ map (mkNode $ gr_links gr)
                                              $ listNodes gr)
                                  <> mkNodeEdges gr

