{-# LANGUAGE ScopedTypeVariables #-}

module RiscV where

import AbsLambda
import ParLambda

import Debug.Trace (traceShowId)
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import qualified Control.Monad.State as State

data Identifier = Identifier String
data CodeState = CodeState { variableMap :: VariableMap }
type VariableMap = Map.Map Ident Int
type Scope = Map.Map Identifier DData
data DData = DBool Bool | DInt Integer | DFunction Ident Expression Scope | DError


type StackPointer = Integer
type Code = [Instruction]
data Instruction = Constant Integer | Plus | VariableReference Int | LetClause Int | CloseScope deriving Show

writeCode :: Int -> Code -> String
writeCode variableCount code = result
	where
		mainInstructions = helper 0 code
		allInstructions = ["addi sp, sp, -48"] ++ initialScope ++ mainInstructions
				++ ["la a0, msg", "ld a1, 0(sp)", "call printf", "li a0, 0", "jal exit"] ++ newScopeFunction
		header = ".section .text\n.globl main\nmain:\n"
		footer = ".section .rodata\nmsg:\n\t\t.string \"Result: %d\\n\"\n "

		result = header ++ (unlines $ map basic_format allInstructions) ++ footer

		basic_format :: String -> String
		basic_format instruction = "\t\t" ++ instruction
	
		helper :: Integer -> Code -> [String]
		helper _ [] = []
		helper stackPointer (Constant value : rest) = (constant stackPointer value) ++ helper (stackPointer + 8) rest
		helper stackPointer (Plus : rest) 			= (plus     stackPointer)       ++ helper (stackPointer - 8) rest
		helper stackPointer (LetClause variableIndex : rest) =
			newScope ++ (letClause stackPointer variableIndex) ++ helper (stackPointer - 8) rest
		helper stackPointer (CloseScope : rest) = closeScope ++ helper stackPointer rest
		helper stackPointer (VariableReference variableIndex : rest) =
			(variableReference stackPointer variableIndex) ++ helper (stackPointer + 8) rest
		helper stackPointer (x : rest) = error $ "unrecognized instruction: " ++ (show x)

		constant :: Integer -> Integer -> [String]
 		constant stackPointer value = [load, store]
 			where
 				load = "li t3, " ++ (show value)
 				store = "sd t3, " ++ (show stackPointer) ++ "(sp)"

 		plus :: Integer -> [String]
 		plus stackPointer =
 			[ "ld t4, " ++ (show (stackPointer - 16)) ++ "(sp)"
			, "ld t5, " ++ (show (stackPointer - 8))  ++ "(sp)"
			, "ADD t3, t4, t5"
			, "sd t3, " ++ (show (stackPointer - 16)) ++ "(sp)"
			]

		variableReference :: Integer -> Int -> [String]
		variableReference stackPointer variableIndex =
			[ "ld t0, -8(sp)" -- current scope
			, "li t1, " ++ (show variableIndex)
			, "addi t1, t1, 1" -- first word of scope is ref to previous scope
			, "slli t1, t1, 3" -- each variable is 8 bytes, so shift left by 3
			, "add t0, t1, t0" -- offset into our scope
			, "ld t2, 0(t0)" -- get the value
			, "sd t2, " ++ (show stackPointer) ++ "(sp)"
			]


		initialScope :: [String]
		initialScope =
			[ "li a0, " ++ (show (8*(variableCount + 2)))
			, "call malloc" -- make space for new scope, assume it works
			, "sd a0, -8(sp)" -- store new scope pointer
			]

		newScope :: [String]
		newScope =
			[ "jal newscope" ]

		newScopeFunction :: [String]
		newScopeFunction =
			[ "newscope:" -- label for the function
			, "li a0, " ++ (show (8*(variableCount + 2))) 
			, "mv s1, ra" -- store return address
			, "call malloc" -- make space for new scope, assume it works
			, "mv ra, s1" -- restore return address
			, "mv t5, a0" -- I'd rather work with the new scope in t5 
			, "ld t4, -8(sp)" -- we'll always keep the most current scope here
			, "sd t5, -8(sp)" -- store new scope pointer
			, "sd t4, 0(t5)" -- store ref to old scope at top of new scope
			, "li a0, " ++ (show variableCount) -- our counter, down to 0-
			, "loop6:"
			, "addi t5, t5, 8" -- start by incrementing pointers to old and new scopes
			, "addi t4, t4, 8"
			, "ld t6, 0(t4)" -- load from old scope into temporary
			, "sd t6, 0(t5)" -- store into new scope from temporary
			, "addi a0, a0, -1" -- decrement loop counter
			, "bgez a0, loop6" -- jump to top of loop if counter is greater than or equal to zero-}
			, "jr ra" -- return from function
			]

		letClause :: Integer -> Int -> [String]
		letClause stackPointer variableIndex =
			["ld t4, " ++ (show (stackPointer - 8)) ++ "(sp)"
			, "li t5, " ++ (show variableIndex)
			, "addi t5, t5, 1" -- first word of scope is ref to prev scope
			, "slli t5, t5, 3" -- each variable is 8 bytes, so shift left by 3
			, "ld t6, -8(sp)" -- this is the scope
			, "add t6, t5, t6" -- we've done the offset into the scope, so now t6 holds the address where the var needs to go
			, "sd t4, 0(t6)" -- do the store!-}
			]

		closeScope :: [String]
		closeScope =
			[ "ld t4, -8(sp)" -- current scope
			, "ld t5, 0(t4)" -- old scope
			, "sd t5, -8(sp)" -- do the store
			]

 		call :: Integer -> [String]
 		call stackPointer =
 			[ "ld t4, " ++ (show (stackPointer - 16)) ++ "(sp)" -- pointer to function
 			, "ld a0, " ++ (show (stackPointer - 8))  ++ "(sp)" -- argument
 			, "call t4" -- this is the function's identifier
 			, "sd a0, " ++ (show (stackPointer - 16)) ++ "(sp)"
 			]

resolve :: Expression -> State.State CodeState Code
resolve s = case s of
	ENum literal -> return [Constant literal]
	ETrue -> return [Constant 1]
	EFalse -> return [Constant 0]
	EPlus expr1 expr2 -> do
		result1 <- resolve expr1
		result2 <- resolve expr2
		return (result1 ++ result2 ++ [Plus])
	EVar ident -> do
		codeState <- State.get
		let varMap = variableMap codeState
		let possibleError = "this variable hasn't been referenced before: (" ++ (show ident) ++ ") varmap: " ++ (show varMap) 
		let realIndex = Maybe.fromMaybe (error possibleError) $ Map.lookup ident varMap
		return [VariableReference realIndex]
	ELet ident value expr -> do
		valueR <- resolve value

		codeState <- State.get
		let varMap = variableMap codeState
		let (possibleNewIndex :: Int) = Map.size varMap
		let (f :: Int -> Int -> Int) = \x -> \y -> y
		let (updatedMap :: VariableMap) = Map.insertWith f ident possibleNewIndex varMap
		State.put $ CodeState updatedMap
		let realIndex = Maybe.fromJust $ Map.lookup ident updatedMap

		exprR  <- resolve expr
		return $ valueR ++ [LetClause realIndex] ++ exprR ++ [CloseScope]
	otherwise -> error "whoops, idk how to handle anything else"

resolve2 :: Expression -> (Code, Int)
resolve2 x =(\(a, b) -> (a, Map.size $ variableMap b)) $ State.runState (resolve x) (CodeState Map.empty)

parse :: String -> Either String Expression
parse s = pExpression $ myLexer s

compile :: String -> Either String String
compile s = do
	(parsed :: Expression) <- parse s
	let (code, variableCount) = resolve2 parsed
	let output = writeCode variableCount code
	return output

writeOrErr :: Either String String -> IO ()
writeOrErr (Left s) = error s
writeOrErr (Right s) = writeFile "output.s" s

input = "(let x 3 (let y 6 y))"
--input = "(let x 3 4)"


doThing :: IO ()
doThing = do
    let s = input
    let result = compile s
    writeOrErr result
