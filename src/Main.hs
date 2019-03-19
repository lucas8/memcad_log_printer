
module Main where

import Syntax
import Lexer
import Parser (parseExpr, parseTokens)
import Graphviz
import Data.Text.Lazy

process :: String -> Either String ([Token], [AST])
process input = do
    toks <- parseTokens input
    ast  <- parseExpr toks
    return (toks, ast)

main :: IO ()
main = do
    input <- getContents
    putStrLn $ case process input of
                 Left err       -> "Failed : " ++ err
                 Right (_, ast) -> unpack $ graphToGraphviz $ makeGraph ast
                                 

