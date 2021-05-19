{-# LANGUAGE TemplateHaskell #-}

import Test.Chell
import Interpreter
import AbsLambda
import ParLambda

import qualified Data.Map.Strict

tests_Math :: Suite
tests_Math = suite "math"
    [ test_Addition
    , test_Subtraction
    ]

test_Addition :: Test
test_Addition = assertions "addition" $ do
    $expect (equal (2 + 1) 3)
    $expect (equal (1 + 2) 3)

test_Subtraction :: Test
test_Subtraction = assertions "subtraction" $ do
    $expect (equal (2 - 1) 1)
    $expect (equal (1 - 2) (-1))

tests_Mary :: Suite
tests_Mary = suite "Mary"
	[ test_idk
	]

test_idk :: Test
test_idk = assertions "idk" $ do
	$expect (equal (interpret "(~x. x)") (Right (DFunction (Ident "x") (EVar (Ident "x")) Data.Map.Strict.empty)))
	$expect (equal (interpret "(let x 413 (let y 612 (let app (~x. (~y. x y)) (let const (~y. (~x. y)) ((app (const 1025)) 1111) ))))") (Right (DInt 1025)))
	$expect (equal (interpret "(let app (~x. (~y. x y)) (let const (~y. (~x. y)) (let x 413 (let y 612 ((app (const 1025)) 1111) ))))") (Right (DInt 1025)))
	$expect (equal (interpret "(and true false)") (Right (DBool False)))
	$expect (equal (interpret "(and true 1)") (Right DError))
	$expect (equal (interpret "(and true") (Left "syntax error at end of file"))
	
main :: IO ()
main = defaultMain [tests_Mary]

--main :: IO ()
--main = putStrLn "Test suite not yet implemented"
