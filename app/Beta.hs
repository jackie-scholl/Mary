module Beta where

import AbsLambda
import LexLambda
import ParLambda
import PrintLambda
import ErrM

beta :: Expression -> Expression
beta (Application (Lambda x t) s) = beta $ substitute x s t
beta (Application e1 e2) = Application (beta e1) (beta e2)
beta (Lambda id expr) = Lambda id (beta expr)
beta (ID x) = ID x

-- unsafe parsing
parseu :: String -> Expression
parseu s = case (pExpression $ myLexer s) of
    Bad x -> ID $ Ident "parse_error"
    Ok  x -> x

-- substitute :: id thing_to_insert overall_term 
substitute :: Ident -> Expression -> Expression -> Expression
substitute curId sub (Lambda newId x) =
    if (newId == curId)
        then (Lambda newId x)
        else (Lambda newId (substitute curId sub x))
substitute curId sub (ID newId) = 
    if (newId == curId)
        then (sub)
        else (ID newId)
substitute curId sub (Application e1 e2) = Application (substitute curId sub e1) (substitute curId sub e2) 

lambdasToTildes :: String -> String
lambdasToTildes = map (\x -> if (x=='λ') then '~' else x)


runBeta :: IO ()
runBeta = do
    s <- getLine
    runProgram s
    where
        runProgram s = mapM_ print $ map printTree $ take 5 $ iterate beta $ parseu $ lambdasToTildes s


