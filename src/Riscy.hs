module Riscy where

import Data.Map.Strict ((!), fromList)

basic_format :: String -> String
basic_format instruction = "\t\t" ++ instruction

--infix = "3+(5-6)*(7-8)"
postfix = "3 5 6 - 7 8 - * +"
list_postfix1 = words postfix

run :: IO ()
run = writeFile "output.s" prime

prime :: String
prime = header ++ (unlines $ map basic_format instructions) ++ footer
	where
		instructions = ["addi sp, sp, -48"] ++ special_instructions ++ ["la a0, msg", "ld a1, 0(sp)", "jal ra, printf", "jal exit" {-"li a0, 0", "li a7, 93", "ecall"-}]
		header = ".section .text\n.globl main\nmain:\n"
		footer = ".section .rodata\nmsg:\n\t\t.string \"Result: %d\\n\"\n "
		special_instructions :: [String]
		special_instructions = postfix_to_instructions list_postfix1

operators :: [String]
operators = ["+", "-", "*", "/"]

postfix_to_instructions :: [String] -> [String]
postfix_to_instructions postfix_list = helper 0 postfix_list
	where
		helper :: Int -> [String] -> [String]
		helper _ [] = []
		helper stack_pointer (item : rest)
			| item `elem` operators = (output_operation stack_pointer item) ++ helper (stack_pointer - 8) rest
 			| otherwise 			= (output_constant  stack_pointer item) ++ helper (stack_pointer + 8) rest

 		output_operation :: Int -> String -> [String]
 		output_operation stack_pointer operator = [
 				"ld t4, " ++ (show (stack_pointer - 16)) ++ "(sp)",
 				"ld t5, " ++ (show (stack_pointer - 8))  ++ "(sp)",
 				op2 ++ " t3, t4, t5",
 				"sd t3, " ++ (show (stack_pointer - 16)) ++ "(sp)"]
 			where
 				operations_map = fromList [("+", "ADD"), ("-", "SUB"), ("*", "MUL"), ("/", "DIV")]
 				op2 = operations_map ! operator

 		output_constant :: Int -> String -> [String]
 		output_constant stack_pointer value = [load, store]
 			where
 				load = "li t3, " ++ value
 				store = "sd t3, " ++ (show stack_pointer) ++ "(sp)"


