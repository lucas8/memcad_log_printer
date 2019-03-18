
module Main where

import Syntax
import Lexer
import Parser (parseExpr, parseTokens)
import Control.Monad

process :: String -> Either String ([Token], [AST])
process input = do
    toks <- parseTokens input
    ast  <- parseExpr toks
    return (toks, ast)

main :: IO ()
main = do
    input <- getContents
    case process input of
      Left err          -> putStrLn $ "Failed : " ++ err
      Right (toks, ast) -> do putStrLn $ "Tokens : " ++ show toks
                              forM_ ast $ putStrLn . show
                                 

