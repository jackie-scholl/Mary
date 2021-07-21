module Interpreter where

import ParLambda
import AbsLambda

import Data.Map.Strict
import Control.Monad.Reader

type Scope = Map Ident DData
data DData = DBool Bool | DInt Integer | DFunction Ident Expression Scope | DError deriving (Eq)

instance Show DData where
	show (DBool True)  = "1"
	show (DBool False) = "0"
	show (DInt x) = show x
	show (DFunction _ _ _) = "function"
	
	

simpleNot :: DData -> DData
simpleNot (DBool b) = DBool $ not b
simpleNot _ = DError

booleanOp :: (Bool -> Bool -> Bool) -> DData -> DData -> DData
booleanOp op (DBool a) (DBool b) = DBool $ a `op` b
booleanOp _ _ _ = DError 

plusOp :: DData -> DData -> DData
plusOp (DInt a) (DInt b) = DInt $ a + b
plusOp _ _ = DError

ifThenElse :: DData -> DData -> DData -> DData
ifThenElse (DBool True)  thenClause _ = thenClause
ifThenElse (DBool False) _ elseClause = elseClause
ifThenElse _ _ _ = DError

resolveCall :: DData -> DData -> DData
resolveCall (DFunction argName functionBody scope) argument = runReader (resolve functionBody) (insert argName argument scope)
resolveCall _ _ = DError

resolve :: Expression -> Reader Scope DData
resolve s = case s of
    ETrue  -> return $ DBool True
    EFalse -> return $ DBool False
    ENum literal -> return $  DInt literal
    EPlus expr1 expr2 -> do
		result1 <- resolve expr1
		result2 <- resolve expr2
		return $ plusOp result1 result2
    EVar ident -> do
        scope <- ask
        return $ scope ! ident
    ENot expr1 -> do
        result1 <- resolve expr1
        return $ simpleNot result1
    EAnd expr1 expr2 -> do
        result1 <- resolve expr1
        result2 <- resolve expr2
        return $ booleanOp (&&) result1 result2
    EOr  expr1 expr2 -> do
        result1 <- resolve expr1
        result2 <- resolve expr2
        return $ booleanOp (||) result1 result2
    EIf  eCondition eThen eElse -> do
        r1 <- resolve eCondition
        r2 <- resolve eThen
        r3 <- resolve eElse
        return $ ifThenElse r1 r2 r3
    ELet ident value expr -> do
        val2 <- resolve value
        let scope3 = insert ident val2
        local scope3 (resolve expr)
    ELambda ident body -> do
        scope <- ask
        return $ DFunction ident body scope
    ECall function argument -> do
        f' <- resolve function
        a' <- resolve argument
        return $ resolveCall f' a'

resolve2 :: Expression -> DData
resolve2 x = runReader (resolve x) empty

interpret :: String -> Either String DData
interpret = fmap resolve2 . pExpression . myLexer
