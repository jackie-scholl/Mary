{-# LANGUAGE ScopedTypeVariables #-}

module RiscV where

import Interpreter (interpret)

import AbsLambda
import ParLambda

--import Debug.Trace (traceShowId)
import qualified Data.Maybe as Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Either as Either
import qualified Control.Monad.State as State
import System.Environment (getArgs)

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
		allInstructions = initialScope ++ mainInstructions
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
			, "ld t2, " ++ (indexToOffset variableIndex) ++ "(t0)" -- get the value
			, "sd t2, " ++ (show stackPointer) ++ "(sp)"
			]

		indexToOffset :: Int -> String
		indexToOffset x = show $ (x + 1)*8

		initialScope :: [String]
		initialScope =
			[ "li a0, " ++ (show $ 1024 * 1024) -- new frame pointer or whatever
			, "call malloc"
			, "mv sp, a0" -- new frame pointer
			, "addi sp, sp, 8" -- for the scope pointer I guess?
			, "li a0, " ++ (show (8*(variableCount + 2)))
			, "call malloc" -- make s pace for new scope, assume it works
			, "sd a0, -8(sp)" -- store new scope pointer
			]

		newScope :: [String]
		newScope =
			[ "jal newscope" ]

		newScopeFunction :: [String]
		newScopeFunction =
			[ "newscope:" -- label for the function

			--malloc section			
			, "li a0, " ++ (show (8*(variableCount + 2))) -- number of bytes we want
			, "mv s1, ra" -- store return address
			, "call malloc" -- make s pace for new scope, assume it works
			, "mv ra, s1" -- restore return address
			, "mv t1, a0" -- I'd rather work with the new scope in t1 

			, "ld t0, -8(sp)" -- we'll always keep the most current scope here
			, "sd t1, -8(sp)" -- store new scope pointer
			, "sd t0, 0(t1)" -- store ref to old scope at top of new scope
			
			, "li t3, " ++ (show variableCount) -- our counter, down to 0-
			, "loop6:"
			, "addi t0, t0, 8" -- start by incrementing pointers to old and new scopes
			, "addi t1, t1, 8"
			, "ld t4, 0(t0)" -- load from old scope into temporary
			, "sd t4, 0(t1)" -- store into new scope from temporary
			, "addi t3, t3, -1" -- decrement loop counter
			, "bgez t3, loop6" -- jump to top of loop if counter is greater than or equal to zero-}
			, "jr ra" -- return from function
			]

		letClause :: Integer -> Int -> [String]
		letClause stackPointer variableIndex =
			["ld t1, " ++ (show (stackPointer - 8)) ++ "(sp)"
			, "ld t0, -8(sp)" -- this is the scope
			, "sd t1, " ++ (indexToOffset variableIndex) ++ "(t0)" -- do the store
			]

		closeScope :: [String]
		closeScope =
			[ "nop"
			, "ld t4, -8(sp)" -- current scope
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

fromRight :: Either a b -> b
fromRight x = Maybe.fromJust $ Either.either (const Nothing) Just x

doThing :: IO ()
doThing = do
	args <- getArgs
	let testName = Maybe.fromJust $ Maybe.listToMaybe args
	let inFileName = "test_data/lambda_code/" ++ testName
	inputString <- readFile inFileName
	let interpreted = show $ fromRight $ interpret inputString
	let interpreted2 = "Result: " ++ interpreted ++ "\n"
	writeFile ("test_data/interpret_out/" ++ testName ++ ".s.out") interpreted2
	
	let result = fromRight $ compile inputString
	writeFile ("test_data/assembly/" ++ testName ++ ".s") result
