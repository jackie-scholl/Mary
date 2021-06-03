module RiscV where

import AbsLambda
import ParLambda

import qualified Data.Map.Strict as Map
import Control.Monad.Reader

data Identifier = Identifier String
type Scope = Map.Map Identifier DData
data DData = DBool Bool | DInt Integer | DFunction Ident Expression Scope | DError

{-
simpleNot :: DData -> DData
simpleNot (DBool b) = DBool $ not b
simpleNot _ = DError

booleanOp :: (Bool -> Bool -> Bool) -> DData -> DData -> DData
booleanOp op (DBool a) (DBool b) = DBool $ a `op` b
booleanOp _ _ _ = DError 

ifThenElse :: DData -> DData -> DData -> DData
ifThenElse (DBool True)  thenClause _ = thenClause
ifThenElse (DBool False) _ elseClause = elseClause
ifThenElse _ _ _ = DError

resolveCall :: DData -> DData -> DData
resolveCall (DFunction argName functionBody scope) argument = runReader (resolve functionBody) (insert argName argument scope)
resolveCall _ _ = DError
-}



type StackPointer = Integer
type Code = [Instruction]
data Instruction = Constant Integer | Plus

{-
outputConstant :: Integer -> State StackPointer Code
outputConstant :: 
outputConstant value = do
		sp <- ask
		let newSp = sp + 8
		let result = helper sp
		local newSp result
	where
		helper :: Integer -> Code
		helper sp = [load, store sp]

		load = Instruction $ "li t3, " ++ (show value)
		store sp = Instruction $ "sd t3, " ++ (show sp) ++ "(sp)" 

outputPlus :: Expression -> Expression -> Reader StackPointer Code
outputPlus = undefined
-}

writeCode :: Code -> String
writeCode code = result
	where
		mainInstructions = helper 0 code
		allInstructions = ["addi sp, sp, -48"] ++ mainInstructions
				++ ["la a0, msg", "ld a1, 0(sp)", "call printf", "li a0, 0", "jal exit"]
		header = ".section .text\n.globl main\nmain:\n"
		footer = ".section .rodata\nmsg:\n\t\t.string \"Result: %d\\n\"\n "

		result = header ++ (unlines $ map basic_format allInstructions) ++ footer

		basic_format :: String -> String
		basic_format instruction = "\t\t" ++ instruction
	
		helper :: Integer -> Code -> [String]
		helper _ [] = []
		helper stackPointer ((Constant value) : rest) = (outputConstant stackPointer value) ++ helper (stackPointer + 8) rest
		helper stackPointer (Plus : rest) 			  = (outputPlus     stackPointer)       ++ helper (stackPointer - 8) rest

		outputConstant :: Integer -> Integer -> [String]
 		outputConstant stackPointer value = [load, store]
 			where
 				load = "li t3, " ++ (show value)
 				store = "sd t3, " ++ (show stackPointer) ++ "(sp)"

 		outputPlus :: Integer -> [String]
 		outputPlus stackPointer = [
 		 				"ld t4, " ++ (show (stackPointer - 16)) ++ "(sp)",
 		 				"ld t5, " ++ (show (stackPointer - 8))  ++ "(sp)",
 		 				"ADD t3, t4, t5",
 		 				"sd t3, " ++ (show (stackPointer - 16)) ++ "(sp)"]

		outputLambda :: Integer -> Ident -> Body -> [String]
		
-- a function is a piece of code that's called with `call`, and has `a0 available to it`
-- it should not access higher stack elements
-- it should save ra for higher use
-- it might be on the heap? at least in some cases
-- (let const (lambda (y) (lambda (x) y)))

 		outputCall :: Integer -> [String]
 		outputCall stackPointer = [
 			"ld t4, " ++ (show (stackPointer - 16)) ++ "(sp)", -- pointer to function
 			"ld a0, " ++ (show (stackPointer - 8))  ++ "(sp)", -- argument
 			"call t4", -- this is the function's identifier
 			"sd a0, " ++ (show (stackPointer - 16)) ++ "(sp)"	
 		]


--resolve :: Expression -> Reader StackPointer Code
resolve :: Expression -> Reader Scope Code
resolve s = case s of
	ENum literal -> return [Constant literal]
	ETrue -> return [Constant 1]
	EFalse -> return [Constant 0]
	EPlus expr1 expr2 -> do
		result1 <- resolve expr1
		result2 <- resolve expr2
		return (result1 ++ result2 ++ [Plus])
	--(resolve expr1) ++ (resolve expr2) ++ [Plus]
	otherwise -> error "whoops, idk how to handle anything else"

resolve2 :: Expression -> Code
resolve2 x = runReader (resolve x) Map.empty

{-
resolve :: Expression -> Reader Scope DData
resolve s = case s of
    ETrue  -> return $ DBool True
    EFalse -> return $ DBool False
    ENum literal -> return $  DInt literal
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
-}

parse :: String -> Either String Expression
parse s = pExpression $ myLexer s

compile :: String -> Either String String
compile = fmap writeCode . fmap resolve2 . parse

writeOrErr :: Either String String -> IO ()
writeOrErr (Left s) = error s
writeOrErr (Right s) = writeFile "output.s" s

input = "true"

doThing :: IO ()
doThing = do
    --s <- getLine
    let s = input
    let result = compile s
    writeOrErr result
    --print $ fmap (\x -> runReader (resolve x) 0) $ pExpression $ myLexer s
    --print $ fmap writeCode $ fmap resolve $ pExpression $ myLexer s
