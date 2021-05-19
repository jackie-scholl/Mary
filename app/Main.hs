module Main where

import AbsLambda
import ParLambda
import Interpreter

main :: IO ()
main = do
    s <- getLine
    print $ fmap resolve2 $ pExpression $ myLexer s
